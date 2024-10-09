package latice.game_manager;

import java.util.ArrayList;
import java.util.List;

import latice.model.GameActionResult;
import latice.model.Player;
import latice.model.tile.Tile;

public abstract class GameManager {
	
	//The event handler used for callbacks
	private static IGameEvents events;
	
	//The player currently playing the round
	private static Player currentPlayer;
	
	
	
	
	/**
	 * 
	 * @param events The event handler to use for callbacks
	 */
	public static void registerEvents(IGameEvents events) {GameManager.events = events;}
	
	/**
	 * 
	 * @return The player currently playing the round
	 */
	public static Player currentPlayer() {return GameManager.currentPlayer;}
	
	/**
	 * 
	 * @param row A row on the gameboard
	 * @param column A row on the gameboard
	 * @param tile The tile to drop
	 * @return The result of the action
	 */
	public static GameActionResult tryPlaceTileAt(int row, int column, Tile tile) {
		return events.onPlayerTryPlaceTileAt(row, column, tile);
	}
	
	
	/**
	 * 
	 * @param player The player who will play on the round
	 */
	public static void startRound(Player player) {
		currentPlayer = player;
		events.onPlayerBeginRound(currentPlayer);
	}
	
	
	/**
	 * Launch a game
	 */
	public static void launchGame() {
		Player player = events.onGameLaunched();
		startRound(player);
	}
	
	/**
	 * Shuffle the tiles
	 * @param listTile The tiles to shuffle
	 */
	public static void mixTiles(List<Tile> listTile ) {		
		int i;
		for (i=0;i<listTile.size();i++) {
			Tile temp1;
			Tile temp2;
			temp1 = listTile.get(i);
			int random = (int) (Math.random() * ( listTile.size() ));
			temp2 = listTile.get(random);
			listTile.set(i, temp2);
			listTile.set(random, temp1);
		}
	}
	
	
	/**
	 * Give players their pool of tiles
	 * @param tileList The tiles to give to the players
	 * @param p1
	 * @param p2
	 */
	public static void givePool(List<Tile> tileList, Player p1, Player p2) {
		int i;	
		List<Tile> poolP1 = new ArrayList<>();
		List<Tile> poolP2 = new ArrayList<>();
		for (i=0;i<tileList.size();i++) {
			if((i<tileList.size()/2)){
				poolP1.add(tileList.get(i));
			}else {
				poolP2.add(tileList.get(i));
			}
		}	
		
		p1.pool(poolP1);
		p2.pool(poolP2);
	}
			
}

