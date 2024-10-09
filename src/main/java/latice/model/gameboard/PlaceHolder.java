package latice.model.gameboard;

import javafx.scene.image.Image;
import latice.model.tile.Tile;

public class PlaceHolder {
	
	public static final Integer WIDTH = 500;
	public static final Integer HEIGHT = 800;

	private final PlaceHolderType type;
	private Tile tile;
	private final Image image;
	
	public PlaceHolder(PlaceHolderType type, Tile tile, Image image) {
		this.type = type;
		this.tile = tile;
		this.image = image;
	}
	
	public PlaceHolder(PlaceHolderType type, Image image) {
		this(type, null, image);
	}
	
	/**
	 * 
	 * @return
	 * The type of this PlaceHolder.
	 */
	public PlaceHolderType type() {return this.type;}
	
	public Tile tile() {return this.tile;}
	
	public void tile(Tile tile) {this.tile = tile;}
	
	public Image image() {return this.image;}
	
	
	/**
	 * 
	 * @return
	 * true if a tile is present.
	 * false otherwise.
	 */
	public Boolean hasTile() {return this.tile != null;}



	public void removeTile() {
		if (Boolean.TRUE.equals(hasTile())) {
			this.tile = null;
		}
	}
}