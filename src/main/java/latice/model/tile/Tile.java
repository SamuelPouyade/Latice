package latice.model.tile;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javafx.scene.image.Image;

public class Tile {
	
	private final TileType type;
	private final TileColor color;
	private final Image image;
	
	public Tile(TileType type, TileColor color, Image image) {
		this.type = type;
		this.color = color;
		this.image = image;
	}
	
	public static List<Tile> createTiles() {
		List<Tile> tiles = new ArrayList<>();
		
		for (TileType type : TileType.values()) {
			for (int i=0; i<2;i++) {
				for (TileColor color : TileColor.values()) {
					String fileName = type.name().toLowerCase()+"_"+color.name().toLowerCase()+".png";
					
					InputStream stream = Tile.class.getResourceAsStream("/images/"+fileName);
					Tile tile = new Tile(type,color, new Image(stream));
					tiles.add(tile);
				}
			}
	}
		
		return tiles;
	}

	public TileType getType() {
		return type;
	}

	public TileColor getColor() {
		return color;
	}
	
	public Image getImage() {return this.image;}
	
	
	
}