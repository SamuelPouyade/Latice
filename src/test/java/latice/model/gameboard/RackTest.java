package latice.model.gameboard;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import javafx.scene.image.Image;
import latice.model.tile.Rack;
import latice.model.tile.Tile;
import latice.model.tile.TileColor;
import latice.model.tile.TileType;

public class RackTest {

	
	@Test
	void doitAjouterUneTuileAuRack() {
		Tile tile = new Tile(TileType.BIRD,TileColor.BLUE,null);
		
		Rack rack = new Rack();
		
		rack.add(tile);
		assertEquals(1, rack.getLength());
	}
	
	@Test
	void doitRetirerUneTuileDuRack() {
		Tile tile = new Tile(TileType.DOLPHIN,TileColor.BLUE,null);
		
		Rack rack = new Rack();
		
		rack.add(tile);
		rack.remove(0);
		assertEquals(0, rack.getLength());
	}
}
