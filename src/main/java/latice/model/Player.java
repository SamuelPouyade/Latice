package latice.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import latice.model.tile.Rack;
import latice.model.tile.Tile;

public class Player {
	private static final Integer DEFAULT_STARTING_POINTS = 2;
	
	
	private Integer points;
	private List<Tile> pool;
	private Rack rack;
	private boolean hasMadeActions = false;
	
	public Player() {
		this.points = DEFAULT_STARTING_POINTS;
		this.pool = new ArrayList<>();
		this.rack = new Rack();
	}
	
	
	public Integer points() {return this.points;}
	public void points(Integer points) {this.points = points;}
	
	public void pool(List<Tile> pool) {this.pool = pool;}
	
	public Rack getRack() {
		return rack;
	}
	
	public void hasMadeActions(boolean madeActions) {this.hasMadeActions=madeActions;}
	public boolean hasMadeActions() {return this.hasMadeActions;}
	
	public void setupRack(int rackSize) {
		Random random = new Random();
		int i = this.rack.getLength();
		while (i<rackSize && !this.pool.isEmpty()) {
			int randomIndex = random.nextInt(this.pool.size());
			Tile tile = this.pool.get(randomIndex);
			this.rack.add(tile);
			this.pool.remove(tile);
			i++;
		}
	}


	public List<Tile> getPool() {
		return pool;
	}
	
	public void removeTileFromRack(int index) {
		this.rack.remove(index);
	}
	
}
