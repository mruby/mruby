include Carbuncle

require 'scripts/player'

class TestGame < Carbuncle::Game
  def load
    @t = 0
    load_music
    load_background
    load_text
    load_player
  end

  def load_music
    @music = Music.new('music/test.ogg')
    @music.play
  end

  def load_background
    texture = Texture.new('graphics/backgrounds/bg.jpg')
    @planes = 5.times.map do |i|
      p = Plane.new(texture)
      p.opacity = 255 - i * 20
      p
    end
  end

  def load_text
    Font.default_name = 'fonts/pixel-unicode.ttf'
    Font.default_size = 64
    @text = ShadowText.new
    @text.value = 'Welcöme to Carbüncle'
    @text.color.set(130, 200, 150)
    @text.position.set(
      (screen.width - @text.width) / 2,
      (screen.height - @text.height) / 2
    )
    @help = ShadowText.new(Font.new('fonts/pixel-unicode.ttf', 24))
    @help.value = "Move with left or right, press 'a' to attack, 's' for screenshot."
    @help.position.set(
      (screen.width - @help.width) / 2,
      (screen.height - @help.height)
    )
  end

  def load_player
    @player = Player.new
    @player.position.set(
      screen.width / 2,
      @text.position.y + @text.height + @player.height
    )
  end

  def update(dt)
    @planes.each.with_index do |p, index|
      p.update(dt)
      p.ox += (2 - index) * dt * 20
      p.oy += (3 - index) * dt * 20
    end
    @text.update(dt)
    @player.update(dt)
    @help.update(dt)
    take_screenshot if Keyboard.press?(:s)
    @t = (@t + dt) % 2
    @text.value = @t > 1 ? 'Welcöme to Carbüncle' : 'Welcome to Carbuncle'
  end

  def take_screenshot
    Bitmap.screenshot.save('screen.png')
  end

  def draw
    @planes.each(&:draw)
    @player.draw
    @text.draw
    @help.draw
  end
end
