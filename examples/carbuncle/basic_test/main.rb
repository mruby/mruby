require 'scripts/player'

class TestGame < Carbuncle::Game
  attr_reader :player

  def load
    @player = Player.new
    @player.position.set(player_x, player_y)
  end

  def player_x
    (screen.width - player.width) / 2
  end

  def player_y
    (screen.height + player.height) / 2
  end

  def update(dt)
    player.update(dt)
  end

  def draw
    player.draw
  end
end
