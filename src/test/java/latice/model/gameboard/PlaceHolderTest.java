package latice.model.gameboard;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import latice.model.tile.Tile;
import latice.model.tile.TileColor;
import latice.model.tile.TileType;

public class PlaceHolderTest {
	
	private GameBoard gameboard;
	
	private final int MIDDLE_ROW = GameBoard.ROWS/2;
	private final int MIDDLE_COLUMN = GameBoard.COLUMNS/2;
	
	private final int START = 0;
	
	private final Tile TILE = new Tile(TileType.BIRD, TileColor.GREEN, null);
	
	@BeforeEach
	public void resetGameBoard() {
		gameboard = GameBoard.loadDefault();
	}
	
	
	@Test
	void should_be_a_moon_square(){
		PlaceHolder placeHolder = gameboard.getPlaceHolderAt(MIDDLE_ROW, MIDDLE_COLUMN);
		
		assertEquals(true, placeHolder.type().equals(PlaceHolderType.MOON_SQUARE));
	}
	
	@Test
	void should_be_a_sun_square() {
		PlaceHolder placeHolder = gameboard.getPlaceHolderAt(START, START);
		
		assertEquals(true, placeHolder.type().equals(PlaceHolderType.SUN_SQUARE));
	}
	
	@Test
	void should_be_an_empty_square() {
		PlaceHolder placeHolder = gameboard.getPlaceHolderAt(START, START+1);
		
		assertEquals(true, placeHolder.type().equals(PlaceHolderType.EMPTY_SQUARE));
	}
	
	@Test
	void should_not_contain_tile() {
		PlaceHolder placeHolder = gameboard.getPlaceHolderAt(MIDDLE_ROW, MIDDLE_COLUMN);
		
		assertEquals(false, placeHolder.hasTile());
	}
	
	@Test
	void should_contain_tile(){
		PlaceHolder placeHolder = gameboard.getPlaceHolderAt(MIDDLE_ROW, MIDDLE_COLUMN);
		
		placeHolder.tile(TILE);
		
		assertEquals(true, placeHolder.hasTile());
		assertEquals(TILE, placeHolder.tile());
	}
	
	@Test
	void should_remove_tile_when_present() {
		PlaceHolder placeHolder = gameboard.getPlaceHolderAt(MIDDLE_ROW, MIDDLE_COLUMN);
		placeHolder.tile(TILE);
		
		assertEquals(true, placeHolder.hasTile());
		assertEquals(TILE, placeHolder.tile());
		
		placeHolder.removeTile();
		
		assertEquals(false, placeHolder.hasTile());
		assertEquals(null, placeHolder.tile());
	}
	
	

}
