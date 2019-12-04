require 'scripts/player'

class TestGame < Carbuncle::Game
  attr_reader :player, :text, :music, :sound

  def load
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
    player.update(dt)
    text.update(dt)
    sound.play if Carbuncle::Keyboard.press?(:a)
  end

  def draw
    player.draw
    text.draw
  end
end
