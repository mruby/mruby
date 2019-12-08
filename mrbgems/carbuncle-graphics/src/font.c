#include "carbuncle/core.h"
#include "carbuncle/font.h"
#include "carbuncle/point.h"

#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/string.h>
#include <mruby/array.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#include "microutf8.h"
#include "microutf8.h"

#include <stdio.h>
#include <string.h>
#include <math.h>

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
    UnloadTexture(font->atlas.texture);
    free_glyph(mrb, font->glyphs.root);
    FT_Done_Face(font->face);
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
  y->height = tree_height(y); 
  x->height = tree_height(x); 
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
  x->height = tree_height(x); 
  y->height = tree_height(y); 
  // Return new root 
  return y; 
}


static struct mrb_Glyph *
balance(struct mrb_Glyph *current)
{
  mrb_int lh = current->left ? current->left->height : 0;
  mrb_int rh = current->right ? current->right->height : 0;
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
set_node_data(struct mrb_Glyph *node, FT_BitmapGlyph bmp)
{
  node->advance.x = bmp->root.advance.x;
  node->advance.x = bmp->root.advance.y;
  node->margin.x = bmp->left;
  node->margin.y = bmp->top;
}

static inline struct mrb_Glyph *
add_node(mrb_state *mrb, struct mrb_Glyph *current, FT_ULong codepoint, FT_BitmapGlyph bmp)
{
  if (!current)
  {
    current = mrb_malloc(mrb, sizeof *current);
    if (!current) { glyph_error(mrb); }
    current->left = NULL;
    current->right = NULL;
    current->codepoint = codepoint;
    current->bmp = bmp;
    current->height = 1;
    set_node_data(current, bmp);
    return current;
  }
  if (current->codepoint == codepoint) { return current; }
  if (current->codepoint < codepoint)
  {
    current->left = add_node(mrb, current->left, codepoint, bmp);
    current->height = tree_height(current);
    return balance(current); 
  }
  current->right = add_node(mrb, current->right, codepoint, bmp);
  current->height = tree_height(current);
  return balance(current);
}

static inline mrb_int
min(mrb_int a, mrb_int b)
{
  return a < b ? a : b;
}

#include <assert.h>

static inline FT_BitmapGlyph
load_glyph(mrb_state *mrb, struct mrb_Font *font, FT_ULong codepoint)
{
  FT_Glyph glyph;
  FT_Matrix matrix = (FT_Matrix){ .xx = 0x10000, .xy = 0, .yx = 0, .yy = 0x10000 };
	FT_Vector pen = (FT_Vector){ .x = 0, .y = 0};
  FT_Set_Transform(font->face, &matrix, &pen);
  if (FT_Load_Char(font->face, codepoint, FT_LOAD_TARGET_NORMAL)) { glyph_error(mrb); }
  if (FT_Get_Glyph(font->face->glyph, &glyph)) { glyph_error(mrb); }
  if (FT_Glyph_To_Bitmap(&glyph, FT_RENDER_MODE_NORMAL, 0, 1)) { glyph_error(mrb); }
  FT_BitmapGlyph bmp = (FT_BitmapGlyph)glyph;
  font->glyphs.root = add_node(mrb, font->glyphs.root, codepoint, bmp);
  return bmp;
}

static inline size_t
draw_glyph(struct mrb_Glyph *glyph, size_t offset, Color *colors, size_t total_width)
{
  
  if (!glyph) { return offset; }
  offset = draw_glyph(glyph->left, offset, colors, total_width);
  offset = draw_glyph(glyph->right, offset, colors, total_width);
  for (size_t y = 0; y < glyph->bmp->bitmap.width; ++y)
  {
    for (size_t i = 0; i < glyph->bmp->bitmap.rows; ++i)
    {
      size_t x = i + offset;
      colors[x + y * total_width] = (Color){
        255, 255, 255, glyph->bmp->bitmap.buffer[i + y * glyph->bmp->bitmap.rows]
      };
    }
  }
  glyph->rect = (Rectangle) {
    offset, 0, glyph->bmp->bitmap.width, glyph->bmp->bitmap.rows
  };
  return offset + glyph->bmp->bitmap.width;
}

static inline void
build_font_atlas(mrb_state *mrb, struct mrb_Font *font)
{
  size_t size = font->metrics.total_width * font->metrics.max_height * sizeof(Color *);
  Color *colors = mrb_malloc(mrb, size);
  memset(colors, 0, size);
  draw_glyph(font->glyphs.root, 0, colors, font->metrics.total_width);
  Image img = LoadImageEx(colors, font->metrics.total_width, font->metrics.max_height);
  font->atlas.texture = LoadTextureFromImage(img);
  UnloadImage(img);
  mrb_free(mrb, colors);
}

static inline void
load_glyphs(mrb_state *mrb, struct mrb_Font *font)
{
  size_t min_h, max_h, w, index, total_w;
  FT_ULong character = FT_Get_First_Char(font->face, &index);
  FT_BitmapGlyph bmp = load_glyph(mrb, font, character);
  w = bmp->bitmap.width;
  min_h = bmp->bitmap.rows;
  max_h = min_h;
  total_w = w;
  character = FT_Get_Next_Char(font->face, character, &index);
  while (character)
  {
    bmp = load_glyph(mrb, font, character);
    size_t new_h = bmp->bitmap.rows;
    total_w += bmp->bitmap.width;
    w = w < bmp->bitmap.width ? bmp->bitmap.width : w;
    min_h = min_h > new_h ? new_h : min_h;
    max_h = max_h < new_h ? new_h : max_h;
    character = FT_Get_Next_Char(font->face, character, &index);
  }
  font->metrics.max_width  = w;
  font->metrics.min_height = min_h;
  font->metrics.max_height = max_h;
  font->metrics.total_width = total_w;
  build_font_atlas(mrb, font);
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
  struct mrb_Font *data = mrb_carbuncle_get_font(mrb, self);
  mrb_get_args(mrb, "z", &text);
  size_t len = utf8_strlen(text);
  FT_BitmapGlyph *bmps = mrb_carbuncle_font_load_glyphs(mrb, data->face, len, text);
	Vector2 size = mrb_carbuncle_font_calculate_size(len, bmps);
  mrb_carbuncle_font_destroy_glyphs(mrb, len, bmps);
  return mrb_carbuncle_point_new(mrb, size.x, size.y);
}

void
mrb_carbuncle_font_init(mrb_state *mrb)
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

FT_Face
mrb_carbuncle_font_get_face(struct mrb_Font *font)
{
  return font->face;
}



FT_BitmapGlyph *
mrb_carbuncle_font_load_glyphs(mrb_state *mrb, FT_Face face, size_t len, const char *message)
{
  FT_UInt codepoint;
  FT_Glyph glyph;
  FT_Matrix matrix = (FT_Matrix){ .xx = 0x10000, .xy = 0, .yx = 0, .yy = 0x10000 };
	FT_Vector pen = (FT_Vector){ .x = 0, .y = 0};
  FT_BitmapGlyph *bmps = mrb_malloc(mrb, len * sizeof(*bmps));
  for (size_t i = 0; i < len; ++i)
  {
    FT_Set_Transform(face, &matrix, &pen);
    message = utf8_decode(message, &codepoint);
    if (FT_Load_Char(face, codepoint, FT_LOAD_TARGET_NORMAL)) { glyph_error(mrb); }
    if (FT_Get_Glyph(face->glyph, &glyph)) { glyph_error(mrb); }
    FT_Glyph_To_Bitmap(&glyph, FT_RENDER_MODE_NORMAL, 0, 1);
    bmps[i] = (FT_BitmapGlyph)glyph;
    pen.x += face->glyph->advance.x;
    pen.y += face->glyph->advance.y;
  }
  return bmps;
}

Vector2
mrb_carbuncle_font_calculate_size(size_t len, FT_BitmapGlyph *bmps)
{
  Vector2 result = (Vector2){0, 0};
	for (size_t i = 0; i < len; ++i)
  {
    FT_BitmapGlyph bmp = bmps[i];
    float new_width = bmp->bitmap.width + bmp->left;
    float new_height = bmp->bitmap.rows + bmp->top;
		if (result.x < new_width) { result.x = new_width; }
		if (result.y < new_height) { result.y = new_height; }
	}  
  return result;
}

void
mrb_carbuncle_font_destroy_glyphs(mrb_state *mrb, size_t len, FT_BitmapGlyph *bmps)
{
  for (size_t i = 0; i < len; ++i)
  {
    FT_Done_Glyph((FT_Glyph)bmps[i]);
  }
  mrb_free(mrb, bmps);
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

struct mrb_Glyph *
mrb_carbuncle_font_get_glyph(struct mrb_Font *font, FT_UInt codepoint)
{
  return find_node(font->glyphs.root, codepoint);
}