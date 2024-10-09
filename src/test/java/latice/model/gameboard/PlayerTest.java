package latice.model.gameboard;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import latice.model.Player;
import latice.model.tile.Rack;
import latice.model.tile.Tile;
import latice.model.tile.TileColor;
import latice.model.tile.TileType;

public class PlayerTest {

	
	@Test
	void DoitretournerUnRackValide() {
		Player player1 = new Player();
		Tile tile = new Tile(TileType.BIRD,TileColor.BLUE,null);
		Tile tile2 = new Tile(TileType.DOLPHIN,TileColor.BLUE,null);
		Tile tile3 = new Tile(TileType.FEATHER,TileColor.BLUE,null);
		Tile tile4 = new Tile(TileType.FLOWER,TileColor.BLUE,null);
		Tile tile5 = new Tile(TileType.TURTLE,TileColor.BLUE,null);
		
		
		player1.getPool().add(tile);
		player1.getPool().add(tile2);
		player1.getPool().add(tile3);
		player1.getPool().add(tile4);
		player1.getPool().add(tile5);
		player1.setupRack(Rack.DEFAULT_RACK_SIZE);
		player1.removeTileFromRack(1);
		
		assertEquals(4, player1.getRack().getLength());
	}
}
