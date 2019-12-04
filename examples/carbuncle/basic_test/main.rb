require 'scripts/player'

class TestGame < Carbuncle::Game
  attr_reader :player, :text, :music, :sound, :plane

  def load
    @plane = Carbuncle::Plane.new(Carbuncle::Texture.new('graphics/backgrounds/bg.png'))
    @music = Carbuncle::Music.new('music/test.ogg')
    @sound = Carbuncle::Sound.new('sound/coin.wav')
    @player = Player.new
    @player.position.set(player_x, player_y)
    @text = Carbuncle::Text.new
    @text.font = Carbuncle::Font.new('fonts/pixel-unicode.ttf', 64)
    @text.value = 'Hëllo Wörld'
    text.position.set((screen.width - text.width) / 2, (screen.height - text.height) / 2)
    music.play
  end

  def player_x
    (screen.width - player.width) / 2
  end

  def player_y
    (screen.height + player.height) / 2
  end

  def update(dt)
    plane.update(dt)
    player.update(dt)
    text.update(dt)
    sound.play if Carbuncle::Keyboard.press?(:a)
    plane.ox += dt * plane.width / 3
    plane.oy += dt * plane.height / 3
  end

  def draw
    plane.draw
    player.draw
    text.draw
  end
end
