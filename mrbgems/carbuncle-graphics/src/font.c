#include "carbuncle/core.h"
#include "carbuncle/font.h"
#include "carbuncle/point.h"
#include "carbuncle/rect.h"

#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/string.h>
#include <mruby/array.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include "microutf8.h"

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>

static const size_t MAX_ATLAS_WIDTH = 2048;

struct mrb_FontCursor
{
  size_t row;
  size_t offset;
};

struct mrb_FontState
{
  struct mrb_Font *font;
  FT_Face face;
  FT_ULong codepoint;
  FT_BitmapGlyph bmp;
  struct mrb_Glyph *current;
  struct mrb_FontCursor cursor;
  struct mrb_FontMetrics metrics;
};

static void
free_glyph(mrb_state *mrb, struct mrb_Glyph *glyph)
{
  if (glyph)
  {
    free_glyph(mrb, glyph->left);
    free_glyph(mrb, glyph->right);
    FT_Done_Glyph((FT_Glyph)glyph->bmp);
    mrb_free(mrb, glyph);
  }
}

static void
mrb_font_free(mrb_state *mrb, void *ptr)
{
  struct mrb_Font *font = ptr;
  if (font)
  {
    UnloadTexture(font->texture);
    UnloadImage(font->image);
    free_glyph(mrb, font->glyphs.root);
    mrb_free(mrb, font);
  }
}

static const struct mrb_data_type font_data_type = {
  "Carbuncle::Font", mrb_font_free
};

static FT_Library carbuncle_freetype;

static void
glyph_error(mrb_state *mrb)
{
  mrb_raise(mrb, mrb->eStandardError_class, "Unable to load font glyphs.");
}

static mrb_int
tree_height(struct mrb_Glyph *node)
{
  if (!node) { return 0; }
  mrb_int left, right;
  left = tree_height(node->left);
  right = tree_height(node->right);
  return 1 + (left > right ? left : right);
}

static struct mrb_Glyph *
rotate_right(struct mrb_Glyph *y)
{ 
  struct mrb_Glyph *x = y->left;
  struct mrb_Glyph *T2 = x->right;
  // Perform rotation 
  x->right = y; 
  y->left = T2; 
  // Update heights 
  y->node_height = tree_height(y); 
  x->node_height = tree_height(x); 
  // Return new root 
  return x; 
} 

static struct mrb_Glyph *
rotate_left(struct mrb_Glyph *x)
{
  struct mrb_Glyph *y = x->right; 
  struct mrb_Glyph *T2 = y->left; 
  // Perform rotation 
  y->left = x; 
  x->right = T2; 
  //  Update heights 
  x->node_height = tree_height(x); 
  y->node_height = tree_height(y); 
  // Return new root 
  return y; 
}


static struct mrb_Glyph *
balance(struct mrb_Glyph *current)
{
  mrb_int lh = current->left ? current->left->node_height : 0;
  mrb_int rh = current->right ? current->right->node_height : 0;
  mrb_int balance = lh - rh;
  if (balance > 0)
  {
    return rotate_right(current);
  }
  else if (balance < 0)
  {
    return rotate_left(current);
  }
  return current;
}

static inline void
set_node_data(struct mrb_Glyph *node, struct mrb_FontState *state)
{
  FT_Face face = state->face;
  FT_BitmapGlyph bmp = state->bmp;
  node->left = NULL;
  node->right = NULL;
  node->codepoint = state->codepoint;
  node->bmp = state->bmp;
  node->node_height = 1;
  node->margin.x = bmp->left;
  node->margin.y = bmp->top;
  if (state->cursor.offset + bmp->bitmap.width > MAX_ATLAS_WIDTH)
  {
    state->cursor.offset = 0;
    state->cursor.row += 1;
  }
  node->rect.x = state->cursor.offset;
  node->row = state->cursor.row;
  node->rect.width = bmp->bitmap.width;
  node->rect.height = bmp->bitmap.rows;
  state->cursor.offset += node->rect.width;
  state->current = node;
  node->advance.x = (bmp->root.advance.x / face->units_per_EM) >> 6;
  node->advance.y = (bmp->root.advance.y / face->units_per_EM) >> 6;
}

static inline struct mrb_Glyph *
add_node(mrb_state *mrb, struct mrb_Glyph *current, struct mrb_FontState *state)
{
  if (!current)
  {
    current = mrb_malloc(mrb, sizeof *current);
    if (!current) { glyph_error(mrb); }
    set_node_data(current, state);
    return current;
  }
  if (current->codepoint == state->codepoint) { return current; }
  if (current->codepoint < state->codepoint)
  {
    current->left = add_node(mrb, current->left, state);
    current->node_height = tree_height(current);
    return balance(current); 
  }
  current->right = add_node(mrb, current->right, state);
  current->node_height = tree_height(current);
  return balance(current);
}

static inline mrb_int
min(mrb_int a, mrb_int b)
{
  return a < b ? a : b;
}

static inline void
load_glyph(mrb_state *mrb, struct mrb_Font *font, struct mrb_FontState *state)
{
  FT_Glyph glyph;
  FT_Matrix matrix = (FT_Matrix){ .xx = 0x10000, .xy = 0, .yx = 0, .yy = 0x10000 };
	FT_Vector pen = (FT_Vector){ .x = 0, .y = 0 };
  FT_Set_Transform(font->face, &matrix, &pen);
  if (FT_Load_Char(font->face, state->codepoint, FT_LOAD_TARGET_NORMAL)) { glyph_error(mrb); }
  if (FT_Get_Glyph(font->face->glyph, &glyph)) { glyph_error(mrb); }
  if (FT_Glyph_To_Bitmap(&glyph, FT_RENDER_MODE_NORMAL, 0, 1)) { glyph_error(mrb); }
  state->bmp = (FT_BitmapGlyph)glyph;
  font->glyphs.root = add_node(mrb, font->glyphs.root, state);
}

static inline void
draw_glyph(struct mrb_Glyph *glyph, Color *colors, struct mrb_FontState *state)
{
  if (!glyph) { return; }
  draw_glyph(glyph->left, colors, state);
  draw_glyph(glyph->right, colors, state);
  assert(glyph->rect.x < MAX_ATLAS_WIDTH);
  glyph->rect.y = glyph->row * state->metrics.max_height;
  for (size_t j = 0; j < glyph->bmp->bitmap.rows; ++j)
  {
    for (size_t i = 0; i < glyph->bmp->bitmap.width; ++i)
    {
      size_t dest_x = i + glyph->rect.x;
      size_t dest_y = j + glyph->rect.y;
      unsigned char buffer = glyph->bmp->bitmap.buffer[i + j * glyph->bmp->bitmap.width];
      colors[dest_x + dest_y * MAX_ATLAS_WIDTH] = (Color){
        255, 255, 255, buffer
      };
    }
  }
}

static inline void
build_font_atlas(mrb_state *mrb, struct mrb_Font *font, struct mrb_FontState *state)
{
  size_t height = font->metrics.max_height * (state->cursor.row + 1);
  size_t color_size = MAX_ATLAS_WIDTH * height;
  size_t size = color_size * sizeof(Color *);
  Color *colors = mrb_malloc(mrb, size);
  for (size_t i = 0; i < color_size; ++i)
  {
    colors[i] = (Color){255, 255, 255, 0};
  }
  draw_glyph(font->glyphs.root, colors, state);
  Image img = LoadImageEx(colors, MAX_ATLAS_WIDTH, height);
  font->texture = LoadTextureFromImage(img);
  font->image = img;
  mrb_free(mrb, colors);
}

static inline void
adjust_metrics(struct mrb_FontState *state)
{
  size_t new_width = state->bmp->bitmap.width;
  size_t new_height = state->bmp->bitmap.rows;
  if (new_width > state->metrics.max_width) { state->metrics.max_width = new_width; }
  if (new_height > state->metrics.max_height) { state->metrics.max_height = new_height; }
  if (new_height < state->metrics.min_height) { state->metrics.min_height = new_height; }
}

static inline void
load_glyphs(mrb_state *mrb, struct mrb_Font *font)
{
  struct mrb_FontState state = (struct mrb_FontState){
    .font = font,
    .face = font->face,
    .current = NULL,
    .bmp = NULL,
    .cursor = (struct mrb_FontCursor){
      .offset = 0,
      .row = 0
    },
    .metrics = (struct mrb_FontMetrics){
      .max_width = 0,
      .min_height = 0,
      .max_height = 0
    }
  };
  FT_UInt index;
  state.codepoint = FT_Get_First_Char(font->face, &index);
  load_glyph(mrb, font, &state);
  state.metrics.max_width = state.bmp->bitmap.width;
  state.metrics.max_height = state.bmp->bitmap.rows;
  state.metrics.min_height = state.metrics.max_height;
  state.codepoint = FT_Get_Next_Char(font->face, state.codepoint, &index);
  while (state.codepoint)
  {
    load_glyph(mrb, font, &state);
    adjust_metrics(&state);
    state.codepoint = FT_Get_Next_Char(font->face, state.codepoint, &index);
  }
  font->metrics = state.metrics;
  build_font_atlas(mrb, font, &state);
}

static inline void
open_font(mrb_state *mrb, struct mrb_Font *font, const char *filename, size_t size)
{
  if (FT_New_Face(carbuncle_freetype, filename, 0, &(font->face)))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot load font '%s'.", filename);
  }
  if (FT_Select_Charmap(font->face, FT_ENCODING_UNICODE))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot load font '%s' as unicode.", filename);
  }
  if (FT_Set_Pixel_Sizes(font->face, 0, size))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot set font size for font %s.", filename);
  }
  font->glyphs.root = NULL;
  load_glyphs(mrb, font);
  FT_Done_Face(font->face);
}

static mrb_value
mrb_font_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_int size, argc;
  const char *name = NULL;
  mrb_value font_class = mrb_obj_value(mrb_carbuncle_class_get(mrb, "Font"));
  argc = mrb_get_args(mrb, "|zi", &name, &size);
  if (argc < 1)
  {
    mrb_value default_name = mrb_funcall(mrb, font_class, "default_name", 0);
    if (mrb_nil_p(default_name))
    {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "Cannot load default font when default font is null.");
    }
    name = mrb_str_to_cstr(mrb, default_name);
  }
  if (argc < 2)
  {
    size = mrb_fixnum(mrb_to_int(mrb, mrb_funcall(mrb, font_class, "default_size", 0)));
  }
  struct mrb_Font *font = mrb_malloc(mrb, sizeof *font);
  DATA_PTR(self) = font;
  DATA_TYPE(self) = &font_data_type;
  font->face = NULL;
  font->size = size;
  mrb_carbuncle_check_file(mrb, name);
  open_font(mrb, font, name, size);
  return self;
}

static mrb_value
mrb_font_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

static mrb_value
mrb_font_dispose(mrb_state *mrb, mrb_value self)
{
  struct mrb_Font *font = mrb_carbuncle_get_font(mrb, self);
  mrb_font_free(mrb, font);
  DATA_PTR(self) = NULL;
  return self;
}

static mrb_value
mrb_font_measure_text(mrb_state *mrb, mrb_value self)
{
  const char *text;
  struct mrb_Font *font = mrb_carbuncle_get_font(mrb, self);
  mrb_get_args(mrb, "z", &text);
  Vector2 size = mrb_carbuncle_font_measure_text(font, text);
  return mrb_carbuncle_point_new(mrb, size.x, size.y);
}

static mrb_value
mrb_font_text_rect(mrb_state *mrb, mrb_value self)
{
  const char *text;
  struct mrb_Font *font = mrb_carbuncle_get_font(mrb, self);
  mrb_get_args(mrb, "z", &text);
  Vector2 size = mrb_carbuncle_font_measure_text(font, text);
  return mrb_carbuncle_rect_new(mrb, 0, 0, size.x, size.y);
}

void
mrb_init_carbuncle_font(mrb_state *mrb)
{
  if (FT_Init_FreeType( &carbuncle_freetype ))
  {
    mrb_raise(mrb, mrb->eStandardError_class, "Unable to initialize freetype.");
  }
  struct RClass *font = mrb_carbuncle_define_data_class(mrb, "Font", mrb->object_class);

  mrb_define_method(mrb, font, "initialize", mrb_font_initialize, MRB_ARGS_OPT(2));

  mrb_define_method(mrb, font, "disposed?", mrb_font_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, font, "dispose", mrb_font_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, font, "measure_text", mrb_font_measure_text, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, font, "text_rect", mrb_font_text_rect, MRB_ARGS_REQ(1));
}

struct mrb_Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &font_data_type, struct mrb_Font);
}

mrb_bool
mrb_carbuncle_font_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &font_data_type);
}

static struct mrb_Glyph *
find_node(struct mrb_Glyph *current, FT_UInt codepoint)
{
  while (current)
  {
    if (current->codepoint == codepoint) { return current; }
    if (current->codepoint < codepoint)
    {
      current = current->left;
    }
    else
    {
      current = current->right;
    }
  }
  return current;
}

Vector2
mrb_carbuncle_font_measure_text(struct mrb_Font *font, const char *text)
{
  uint32_t codepoint;
  size_t len = utf8_strlen(text);
  Vector2 size = { 0, 0 };
  Vector2 pen = { 0, 0 };
  for (size_t i = 0; i < len; ++i)
  {
    text = utf8_decode(text, &codepoint);
    struct mrb_Glyph *glyph = mrb_carbuncle_font_get_glyph(font, codepoint);
    if (glyph)
    {
      float new_w = pen.x + glyph->bmp->bitmap.width + glyph->bmp->left;
      float new_h = pen.y + glyph->bmp->bitmap.rows + glyph->bmp->top;
      if (new_w > size.x) { size.x = new_w; }
      if (new_h > size.y) { size.y = new_h; }
      pen.x += glyph->advance.x;
      pen.y += glyph->advance.y;
    }
  }
  return size;
}

struct mrb_Glyph *
mrb_carbuncle_font_get_glyph(struct mrb_Font *font, FT_UInt codepoint)
{
  return find_node(font->glyphs.root, codepoint);
}