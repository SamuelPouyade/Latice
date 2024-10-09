package latice.model.gameboard;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

import javafx.scene.image.Image;
import latice.model.tile.Tile;

public class GameBoard {

	private static final char EMPTY_PLACE_HOLDER_SYMBOL = '#';
	private static final char SUN_PLACE_HOLDER_SYMBOL = '*';
	private static final char MOON_PLACE_HOLDER_SYMBOL = '@';
	private static final String DEFAULT_GAME_BOARD_LOCATION = "/default_gameboard";
	
	public static final Integer ROWS = 9;
	public static final Integer COLUMNS = 9;
	
	private PlaceHolder[][] board = new PlaceHolder[ROWS][COLUMNS];
	
	private GameBoard() {}
	
	/**
	 * 
	 * @return
	 * The default game board or null if an error occurred.
	 */
	public static GameBoard loadDefault(){
		GameBoard gameBoard = new GameBoard();
		
		try(BufferedInputStream in = new BufferedInputStream(GameBoard.class.getResourceAsStream(DEFAULT_GAME_BOARD_LOCATION));) {
			
			
			byte[] data = new byte[in.available()];
			in.read(data);
			
			ByteBuffer buffer = ByteBuffer.wrap(data);
			String text = Charset.defaultCharset().newDecoder().decode(buffer).toString();
			
			
			char[] chars = text.replace(" ", "").toCharArray();
			int row = 0;
			int column = 0;
			for(int i = 0; i < chars.length; i++) {
				Character car = chars[i];
				if(!car.equals('\n')) {
					
					if(car.equals(EMPTY_PLACE_HOLDER_SYMBOL)) {
						gameBoard.board[row][column] = new PlaceHolder(PlaceHolderType.EMPTY_SQUARE, new Image(GameBoard.class.getResourceAsStream("/images/bg_sea.png")));
					}else if(car.equals(SUN_PLACE_HOLDER_SYMBOL)) {
						gameBoard.board[row][column] = new PlaceHolder(PlaceHolderType.SUN_SQUARE, new Image(GameBoard.class.getResourceAsStream("/images/bg_sun.png")));
					}else if(car.equals(MOON_PLACE_HOLDER_SYMBOL)) {
						gameBoard.board[row][column] = new PlaceHolder(PlaceHolderType.MOON_SQUARE, new Image(GameBoard.class.getResourceAsStream("/images/bg_moon.png")));
					}
					
					column++;
					
				}else {
					row++;
					column = 0;
					
				}
				
			}
			
			
		} catch (IOException e) {
			
			e.printStackTrace();
			return null;
		}
		
		
		return gameBoard;
	}
	
	/**
	 * Place a tile at the desired coordinates.
	 * @param row
	 * The row to place the tile.
	 * @param column
	 * The column to place the tile.
	 * @param tile
	 * The tile to be placed.
	 * @return
	 * true if the tile has been placed.
	 * false otherwise.
	 */
	public Boolean placeTileAt(int row, int column, Tile tile) {
		if(Boolean.FALSE.equals(validCoordinates(row, column)))
			return false;
		
		if(Boolean.TRUE.equals(hasTileAt(row, column)))
			return false;
		
		getPlaceHolderAt(row, column).tile(tile);
		return true;
		
	}
	
	/**
	 * 
	 * @param row
	 * A row of the board.
	 * @param column
	 * A column of the board.
	 * @return
	 * true if a tile is present at the desired coordinates.
	 * false otherwise.
	 */
	public Boolean hasTileAt(int row, int column) {
		if(Boolean.FALSE.equals(validCoordinates(row, column)))
			return false;
		
		return getPlaceHolderAt(row, column).hasTile();
	}
	
	/**
	 * 
	 * @param row
	 * A row of the board.
	 * @param column
	 * A column of the board.
	 * @return
	 * The PlaceHolder present at the desired coordinates.
	 * null if the coordinates are invalid.
	 */
	public PlaceHolder getPlaceHolderAt(int row, int column) {
		if(Boolean.FALSE.equals(validCoordinates(row, column)))
			return null;
		
		return this.board[row][column];
	}
	
	
	/**
	 * 
	 * @param row
	 * The row to test.
	 * @param column
	 * The column to test.
	 * @return
	 * true if the coordinates are valid.
	 * false otherwise.
	 */
	public Boolean validCoordinates(int row, int column) {
		return (row >= 0 && row < ROWS) && (column >= 0 && column < COLUMNS);
	}
	
	public void clear () {
		for(int row = 0; row<ROWS;row++) {
			for(int col = 0; col<COLUMNS;col++) {
				this.board[row][col].removeTile();
			}
		}
	}
}
