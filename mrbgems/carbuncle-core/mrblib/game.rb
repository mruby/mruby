module Carbuncle
  class Game
    class << self
      # @!attribute [r] current_game
      #   The current game, that's already running.
      #   @return [Carbuncle::Game]
      # @!method running?
      #   Returns true if a game is running
      #   @return [true, false]
    end

    # @!group Lifecycle methods

    # This method is called after the graphics context is created.
    # This method should be overrided your main screen for your game.
    # @example The game starts
    #   class MyGame < Carbuncle::Game
    #     def load
    #       # The screen is loaded, so we can load our assets
    #       SceneManager.set(TitleScene)
    #     end
    #   end
    # @return [nil]
    def load; end

    # This method is called after an update, usually once per frame.
    # If the application skips frames, this method won't be called.
    # @param [Float] dt The time since the last update.
    # @return [nil]
    def update(dt); end

    # This method is called once per frame. This is inside the drawing context,
    # so you can only draw objects inside this function.
    # @return [nil]
    def draw; end

    # @!group Lifecycle events

    # This event is called before the game starts, even before the screen is initialized.
    # If you require to load your assets, use Carbuncle::Game#load instead.
    # @return [nil]
    def before_run; end

    # This event is called after the game starts, even after load.
    # You can use this event to trigger your start, if you want to keep it separated from load.
    # @return [nil]
    def after_run; end

    # This event is called before the game is closed, useful for cleanup.
    # @return [nil]
    def before_close; end

    # This event is called after the game is closed and the screen is destroyed.
    # @return [nil]
    def after_close; end
  end
end
