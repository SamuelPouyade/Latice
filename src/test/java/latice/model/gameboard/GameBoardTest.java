package latice.model.gameboard;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import latice.model.tile.Tile;
import latice.model.tile.TileColor;
import latice.model.tile.TileType;

public class GameBoardTest {
	
	private final int START = 0;
	private final int ROW_END = GameBoard.COLUMNS-1;
	private final int COLUMN_END = GameBoard.COLUMNS-1;
	
	private final int OUT_OF_BOUNDS_ROW = ROW_END+1;
	private final int OUT_OF_BOUND_COLUMN = COLUMN_END+1;
	
	private final int INVALID_INDEX = -1;
	
	private final Tile TILE = new Tile(TileType.BIRD, TileColor.BLUE, null);
	
	private GameBoard gameboard;
	
	
	@BeforeEach
	public void resetGameBoard() {
		gameboard = GameBoard.loadDefault();
	}
	
	@Test
	public void placeholder_should_not_contain_tile() {
		assertEquals(false, gameboard.hasTileAt(START, START));
		assertEquals(false, gameboard.hasTileAt(ROW_END, COLUMN_END));
		
		
	}
	
	@Test
	public void can_place_tile_when_empty() {
		
		assertEquals(false, gameboard.hasTileAt(ROW_END, COLUMN_END));
		
		boolean result = gameboard.placeTileAt(ROW_END, COLUMN_END, TILE);
		assertEquals(true, result);
		
		
	}
	
	@Test
	public void cannot_place_tile_when_not_empty() {
		assertEquals(false, gameboard.hasTileAt(START, START));
		
		boolean result = gameboard.placeTileAt(START, START, TILE);
		assertEquals(true, result);
		
		result = gameboard.placeTileAt(START, START, TILE);
		assertEquals(false, result);
		
	}
	
	@Test
	public void coordinates_should_be_valid() {
		
		boolean valid = gameboard.validCoordinates(START, START);
		assertEquals(true, valid);
		
		valid = gameboard.validCoordinates(ROW_END, COLUMN_END);
		assertEquals(true, valid);
		
		valid = gameboard.validCoordinates(START, COLUMN_END);
		assertEquals(true, valid);
		
		valid = gameboard.validCoordinates(ROW_END, START);
		assertEquals(true, valid);
	}
	
	@Test
	public void coordinates_should_not_be_valid() {
		boolean valid = gameboard.validCoordinates(OUT_OF_BOUNDS_ROW, OUT_OF_BOUND_COLUMN);
		assertEquals(false, valid);
		
		valid = gameboard.validCoordinates(INVALID_INDEX, INVALID_INDEX);
		assertEquals(false, valid);
		
		valid = gameboard.validCoordinates(START, OUT_OF_BOUND_COLUMN);
		assertEquals(false, valid);
		
		valid = gameboard.validCoordinates(START, INVALID_INDEX);
		assertEquals(false, valid);
	}
	

}
