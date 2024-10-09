package latice.model.tile;

import java.util.ArrayList;
import java.util.List;

public class Rack {
	public static final Integer DEFAULT_RACK_SIZE = 5;
	private List<Tile> tiles = new ArrayList<>();
	
	public List<Tile> getTiles() {
		return this.tiles;
	}

	public void add(Tile tile) {
		this.tiles.add(tile);
	}
	
	public Tile getTile(Integer i) {
		return this.tiles.get(i);
	}

	public void remove(int index) {
		this.tiles.remove(index);
		
	}
	
	public int getLength() {
		return this.tiles.size();
	}

}
