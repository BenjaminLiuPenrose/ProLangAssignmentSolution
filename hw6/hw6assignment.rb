# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.
# require_relative 'hw6provided'

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  #========================================================= Problem 2=============================
  All_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]])] # Z
  All_My_Pieces = All_Pieces + [rotations([[0, 0], [-1, 0], [1, 0], [0, 1], [-1, -1]]), # new shape one
								[[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],
								[[0, 2], [0, 1], [0, 0], [0, -1], [0, -2]]],	# the second new shape(only needs two)
								rotations([[0, 0], [1, 0], [0, 1]])] 			# the third new shape

  #========================================================= Problem 3=============================
  Cheat_Pieces = [[[[0, 0]]]]

  # your enhancements here
  def initialize (point_array, board)
  	super(point_array, board)
  end
  #========================================================= Problem 2=============================
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  #========================================================= Problem 3=============================
  def self.cheat_next_piece (board)
    MyPiece.new(Cheat_Pieces.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
  	super(game)
  	@current_block = MyPiece.next_piece(self)
    @cheat = false
  end
  #========================================================= Problem 3=============================
  def next_piece
    if @cheat == false
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    else
      @current_block = MyPiece.cheat_next_piece(self)
      @current_pos = nil
      @cheat = false
    end
  end
  #========================================================= Problem 3=============================
  def cheating
    if @cheat == false
      if @score >= 100
        @cheat = true
        @score = @score  - 100
      end
    end
  end

  #========================================================= Problem 1=============================
  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

end

class MyTetris < Tetris
  # your enhancements here
  def initialize
  	super()
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    @root.bind('n', proc {self.new_game})

    @root.bind('p', proc {self.pause})

    @root.bind('q', proc {exitProgram})

    @root.bind('a', proc {@board.move_left})
    @root.bind('Left', proc {@board.move_left})

    @root.bind('d', proc {@board.move_right})
    @root.bind('Right', proc {@board.move_right})

    @root.bind('s', proc {@board.rotate_clockwise})
    @root.bind('Down', proc {@board.rotate_clockwise})

    @root.bind('w', proc {@board.rotate_counter_clockwise})
    @root.bind('Up', proc {@board.rotate_counter_clockwise})

    #========================================================= Problem 1=============================
    @root.bind('u', proc {@board.rotate_180_degrees})

    #========================================================= Problem 3=============================
    @root.bind('c', proc {@board.cheating})

    @root.bind('space' , proc {@board.drop_all_the_way})
  end

end


