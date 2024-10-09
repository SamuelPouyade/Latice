package latice.game_manager;

import latice.model.GameActionResult;
import latice.model.Player;
import latice.model.tile.Tile;

public interface IGameEvents {

	/**
	 * Called when a game is launched
	 * @return The first player to play
	 */
	Player onGameLaunched();
	
	/**
	 * Called on the start of a player round
	 * @param player The player playing the current round
	 */
	void onPlayerBeginRound(Player player);
	
	/**
	 * @param row A row on the gameboard
	 * @param column A column on the gameboard
	 * @param tile The tile to drop
	 * @return The result of the action
	 * @see GameActionResult
	 */
	GameActionResult onPlayerTryPlaceTileAt(int row, int column,Tile tile);
	
}
