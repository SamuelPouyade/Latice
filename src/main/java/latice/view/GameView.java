package latice.view;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundImage;
import javafx.scene.layout.BackgroundPosition;
import javafx.scene.layout.BackgroundRepeat;
import javafx.scene.layout.BackgroundSize;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import latice.game_manager.GameManager;
import latice.game_manager.IGameEvents;
import latice.model.GameActionResult;
import latice.model.Player;
import latice.model.gameboard.GameBoard;
import latice.model.gameboard.PlaceHolder;
import latice.model.gameboard.PlaceHolderType;
import latice.model.tile.Rack;
import latice.model.tile.Tile;
import latice.model.tile.TileColor;
import latice.model.tile.TileType;

public class GameView extends ApplicationView implements IGameEvents{
	private static final Integer BTNWIDTH = 900;
	private static final Integer BTNHEIGHT = 800;
	private Player player1;
	private Player player2;
	private final HashMap<Player, Integer> tilesPlacedByPlayer = new HashMap<>();
	private final HBox rackJoueur =  new HBox();
	private final Label remainTiles = new Label();
	private final Label playerName = new Label();
	private final Label playerPoints = new Label();
	private Integer roundLeft;
	
	private GameBoard gameBoard = GameBoard.loadDefault();
	
	
	
	private GridPane grid = new GridPane();
	

	@Override
	protected Scene initializeScene() {
		return new Scene(new BorderPane());
	}

	@Override
	public Boolean setUpView() {
		Image fond = new Image(getClass().getResourceAsStream("/images/laticeBackground.png"));
		GameManager.registerEvents(this);
		
		
		BorderPane root = (BorderPane) this.scene.getRoot();
		
		//Background 
		
		BackgroundSize backgroundSize = new BackgroundSize(100, 100, true, true, true, true);
        
        BackgroundImage backgroundImage = new BackgroundImage(fond,BackgroundRepeat.NO_REPEAT,
                BackgroundRepeat.NO_REPEAT,
                BackgroundPosition.CENTER,
                backgroundSize);
        
        Background background = new Background(backgroundImage);
        
        root.setBackground(background);
		
		
		
		//CENTER
		
		grid.setAlignment(Pos.CENTER);
		root.setCenter(grid);
		
		
		
		
		//RIGHT
		playerName.setStyle("-fx-font: 38 arial;");
		remainTiles.setStyle(("-fx-font: 24 arial;"));
		playerPoints.setStyle("-fx-font: 24 arial;");
		VBox vBoxRight = new VBox();
		vBoxRight.getChildren().addAll(playerName,remainTiles,rackJoueur,playerPoints);
		vBoxRight.setAlignment(Pos.CENTER);
		vBoxRight.setPadding(new Insets(25));
		root.setRight(vBoxRight);
		
		
		//Bottom
		HBox hBoxGame = new HBox();
		Button btnChangeRack = new Button("Changer rack");
		Button btnPassRound = new Button("Passer son tour");
		Button btnLeaveGame = new Button("Quitter");
		
		
		btnChangeRack.setPrefSize(fitToScreenWidth(BTNWIDTH), fitToScreenHeight(BTNHEIGHT));
		btnPassRound.setPrefSize(fitToScreenWidth(BTNWIDTH), fitToScreenHeight(BTNHEIGHT));
		btnLeaveGame.setPrefSize(fitToScreenWidth(BTNWIDTH), fitToScreenHeight(BTNHEIGHT));
		
		
		btnLeaveGame.setOnMouseClicked(event -> 
			ApplicationView.show(HOME_VIEW));
		
		btnChangeRack.setOnMouseClicked(event -> {
			if ((GameManager.currentPlayer().points()-2)<0){
				return;
			}
			for (Tile tile : GameManager.currentPlayer().getRack().getTiles()) {
				GameManager.currentPlayer().getPool().add(tile);
			}
			int rackSize = GameManager.currentPlayer().getRack().getLength();
			GameManager.currentPlayer().getRack().getTiles().clear();
			GameManager.currentPlayer().setupRack(rackSize);
			GameManager.currentPlayer().points(GameManager.currentPlayer().points()-2);
			
			GameManager.currentPlayer().hasMadeActions(true);
			
			updateRack(GameManager.currentPlayer());
			updatePointsCounter();
		});
		
		btnPassRound.setOnMouseClicked(event -> {
			
			GameManager.currentPlayer().setupRack(Rack.DEFAULT_RACK_SIZE);
			
		
			if (roundLeft == 0) {
				Alert alert = new Alert(AlertType.CONFIRMATION);
				
				
				int tilesPlacedBPlayer1 = tilesPlacedByPlayer.getOrDefault(player1, 0);
				int tilesPlacedBPlayer2 = tilesPlacedByPlayer.getOrDefault(player2, 0);
				if(tilesPlacedBPlayer1 > tilesPlacedBPlayer2)
					alert.setHeaderText("Victoire du Joueur 1" + "\n tuiles posée par le Joueur 1: " + tilesPlacedBPlayer1+"\n tuiles posée par le Joueur 2: "+ tilesPlacedBPlayer2);
				else if (tilesPlacedBPlayer1 < tilesPlacedBPlayer2)
					alert.setHeaderText("Victoire du Joueur 2"+ "\n tuiles posée par le Joueur 1: " + tilesPlacedBPlayer1+"\n tuiles posée par le Joueur 2: "+ tilesPlacedBPlayer2);
				else {
					alert.setHeaderText("Egalite"+"\n tuiles posée pour chaque joueur: " + tilesPlacedBPlayer1);
				}
				
				alert.setTitle("Score de chaque Joueur");
				alert.setContentText("Voulez-vous rejouer ? ");
				alert.getButtonTypes().setAll(ButtonType.YES,ButtonType.NO);
				Optional<ButtonType> option = alert.showAndWait();
				
				if(option.isPresent()) {
					if(option.get()==ButtonType.YES)
						GameManager.launchGame();
					else {
						ApplicationView.show(HOME_VIEW);
					}	
				}else
					ApplicationView.show(HOME_VIEW);
			
				
			}else {
				if(GameManager.currentPlayer().hasMadeActions()) {
					GameManager.currentPlayer().points(GameManager.currentPlayer().points()+2);
					GameManager.currentPlayer().hasMadeActions(false);
				}
				
			    if(GameManager.currentPlayer() == player1)
			    	GameManager.startRound(player2);
			    else
			      GameManager.startRound(player1);
			}
			
			
		});
		
		
		
		hBoxGame.getChildren().addAll(btnChangeRack, btnLeaveGame,btnPassRound);
		hBoxGame.setSpacing(10);
		hBoxGame.setAlignment(Pos.BOTTOM_RIGHT);
		
		BorderPane.setMargin(hBoxGame, new Insets(25));
		root.setBottom(hBoxGame);

		
		
		
		
		return true;

	}
	
	private void buildGridPane() {
		for (int i=0;i<GameBoard.ROWS;i++) {
			for (int j=0;j<GameBoard.COLUMNS;j++) {
				PlaceHolder pHolder = gameBoard.getPlaceHolderAt(i, j);
				ImageView imgView = new ImageView(pHolder.image());
				imgView.setFitWidth(fitToScreenWidth(PlaceHolder.WIDTH));
				imgView.setFitHeight(fitToScreenHeight(PlaceHolder.HEIGHT));
				
				grid.add(imgView, i, j);
			}
		}
	}
	
	private void initializeDragAndDrop(ImageView imgView) {
		
		final int rowIndex = GridPane.getRowIndex(imgView);
		final int columnIndex = GridPane.getColumnIndex(imgView);
		
		imgView.setOnDragOver(new EventHandler<DragEvent>() {
			@Override
			public void handle(DragEvent event) {
				if (event.getDragboard().hasString()) {
					String[] info = event.getDragboard().getString().split("_");
					TileType tileType = TileType.valueOf(info[0]);
					TileColor tileColor = TileColor.valueOf(info[1]);
					Tile candidate = new Tile(tileType,tileColor,null);
					
					if(GameManager.tryPlaceTileAt(rowIndex, columnIndex,candidate) == GameActionResult.GRANTED)
						event.acceptTransferModes(TransferMode.ANY);
					
					
				}
				event.consume();
			}
			
		});
		
		imgView.setOnDragDropped(new EventHandler<DragEvent>() {

			@Override
			public void handle(DragEvent event) {
				String[] info = event.getDragboard().getString().split("_");
				
				
				
				String tileType = info[0];
				String tileColor = info[1];
				Image tileImg = new Image(this.getClass().getResourceAsStream("/images/"+tileType.toLowerCase()+"_"+tileColor.toLowerCase()+".png"), fitToScreenWidth(PlaceHolder.WIDTH), fitToScreenHeight(PlaceHolder.HEIGHT),false,false);
				
				Integer tileIndex = Integer.valueOf(info[2]);
				
				int nombrePoints = GameManager.currentPlayer().points();
				
				Tile tile = new Tile(TileType.valueOf(tileType),TileColor.valueOf(tileColor), tileImg);
				gameBoard.placeTileAt(rowIndex, columnIndex, tile);
				
				int tilesPlaced = tilesPlacedByPlayer.getOrDefault(GameManager.currentPlayer(), 0);
				
				tilesPlacedByPlayer.put(GameManager.currentPlayer(), tilesPlaced+1);
				
				nombrePoints -=2;
				
				int nombreTuilesAdjacentes = getAdjacentTiles(rowIndex, columnIndex).size();
				
				if (nombreTuilesAdjacentes == 4 ) {
					nombrePoints += (nombreTuilesAdjacentes);
				}else if (nombreTuilesAdjacentes > 0 ) {
					nombrePoints += (nombreTuilesAdjacentes-1);
				}
				
				
				if (gameBoard.getPlaceHolderAt(rowIndex, columnIndex).type() == PlaceHolderType.SUN_SQUARE) {
					nombrePoints+=2;
				}
				GameManager.currentPlayer().points(nombrePoints);
				GameManager.currentPlayer().hasMadeActions(true);
				
				GameManager.currentPlayer().getRack().remove(tileIndex);
				updateRack(GameManager.currentPlayer());
				updatePointsCounter();
				updateGameBoardVisuals(rowIndex, columnIndex);
				updateGameBoard(rowIndex,columnIndex);
				
			}
			
		});
	}
	
	private ImageView updateGameBoardVisuals(int row, int column) {
		PlaceHolder placeHolder = gameBoard.getPlaceHolderAt(row, column);
		Image img = null;
			
		if (Boolean.TRUE.equals(placeHolder.hasTile())) {
			img = placeHolder.tile().getImage();
		}
		else {
			img = placeHolder.image();
		}
		ImageView imgView = new ImageView(img);
		imgView.setFitWidth(fitToScreenWidth(PlaceHolder.WIDTH));
		imgView.setFitHeight(fitToScreenHeight(PlaceHolder.HEIGHT));
		grid.add(imgView, column, row);
		
		return imgView;
		
		
		
	}
	
	private void updateGameBoard(int row, int column) {
		if (Boolean.TRUE.equals(!gameBoard.hasTileAt(row+1, column))&& Boolean.TRUE.equals(gameBoard.validCoordinates(row+1, column))) {
			initializeDragAndDrop(updateGameBoardVisuals(row+1, column));
		}
		if (Boolean.TRUE.equals(!gameBoard.hasTileAt(row, column+1)) && Boolean.TRUE.equals(gameBoard.validCoordinates(row, column+1))) {
			initializeDragAndDrop(updateGameBoardVisuals(row, column+1));
		}
		if (Boolean.TRUE.equals(!gameBoard.hasTileAt(row, column-1))&& Boolean.TRUE.equals(gameBoard.validCoordinates(row, column-1))) {
			initializeDragAndDrop(updateGameBoardVisuals(row, column-1));
		}
		if (Boolean.TRUE.equals(!gameBoard.hasTileAt(row-1, column))&& Boolean.TRUE.equals(gameBoard.validCoordinates(row-1, column))) {
			initializeDragAndDrop( updateGameBoardVisuals(row-1, column));
		}
	}
	
	private List<Tile> getAdjacentTiles(int row, int column ){
		List<Tile> adjacentTiles = new ArrayList<>();
		//BottomTile
		if (Boolean.TRUE.equals(gameBoard.hasTileAt(row+1, column))) {
			adjacentTiles.add(gameBoard.getPlaceHolderAt(row+1, column).tile());
		}
		//RightTile
		if (Boolean.TRUE.equals(gameBoard.hasTileAt(row, column+1))) {
			adjacentTiles.add(gameBoard.getPlaceHolderAt(row, column+1).tile());
		}
		//LeftTile
		if (Boolean.TRUE.equals(gameBoard.hasTileAt(row, column-1))) {
			adjacentTiles.add(gameBoard.getPlaceHolderAt(row, column-1).tile());
		}
		//TopTile
		if (Boolean.TRUE.equals(gameBoard.hasTileAt(row-1, column))) {
			adjacentTiles.add(gameBoard.getPlaceHolderAt(row-1, column).tile());
		}
		return adjacentTiles;
	}
	
	private boolean checkColorAndType(int row, int column,Tile tile) {
		List<Tile> listAdjacent = getAdjacentTiles(row, column);
		Boolean isValid = true;
		int i = 0;
		while (Boolean.TRUE.equals(isValid) && i< listAdjacent.size()) {
			Tile candidate = listAdjacent.get(i);
			isValid =tile.getColor().equals(candidate.getColor()) || tile.getType().equals(candidate.getType());
			i++;
		}
		return isValid;
	}

	private void clearGameBoard() {
        gameBoard.clear();
        grid.getChildren().clear();
    }
	
	@Override
	protected void onShow() {
		ApplicationView.stage().setMaximized(true);
		GameManager.launchGame();
	}
	
	
	
	private void giveRacks( Player p1, Player p2) {
		List<Tile> tileList = Tile.createTiles();
        GameManager.mixTiles(tileList);
        GameManager.givePool(tileList, p1, p2);
        p1.setupRack(Rack.DEFAULT_RACK_SIZE);
        p2.setupRack(Rack.DEFAULT_RACK_SIZE);
        
        
	}
	
	private void updatePointsCounter() {
		playerPoints.setText("Nombre de points: "+GameManager.currentPlayer().points());
	}
	
	private void updateRack(Player player) {
		rackJoueur.getChildren().clear();
        for (Integer i =0;i<Rack.DEFAULT_RACK_SIZE;i++) {
        	if (i<player.getRack().getLength()) {
	        	Tile tile =player.getRack().getTile(i);
	            ImageView imgTile = new ImageView(tile.getImage());
	            
	            imgTile.setFitWidth(fitToScreenWidth(PlaceHolder.WIDTH));
	            imgTile.setFitHeight(fitToScreenHeight(PlaceHolder.HEIGHT));
	            
	            final Integer index = i; 
	            imgTile.setOnDragDetected(event -> {
	            	Dragboard db = imgTile.startDragAndDrop(TransferMode.ANY);
					
					ClipboardContent content = new ClipboardContent();
					content.putString(tile.getType().toString()+"_"+tile.getColor().toString()+"_"+index.toString());
					db.setContent(content);
					db.setDragView(imgTile.getImage(), fitToScreenWidth(PlaceHolder.WIDTH)/2, fitToScreenHeight(PlaceHolder.HEIGHT)/2);
					
					event.consume();
	            
	            });
	
				rackJoueur.getChildren().add(imgTile);
        	}else {
        		ImageView emptyImageView = new ImageView(new Image(this.getClass().getResourceAsStream("/images/bg_sea.png"), fitToScreenWidth(PlaceHolder.WIDTH), fitToScreenHeight(PlaceHolder.HEIGHT),false,false));
        		rackJoueur.getChildren().add(emptyImageView);
        	}
        }
        
        remainTiles.setText("Tuiles restantes dans le pool: "+player.getPool().size());
        
	}

	@Override
	public Player onGameLaunched() {

		
		roundLeft = 10;
		tilesPlacedByPlayer.clear();
		player1 = new Player();
		player2 = new Player();
		giveRacks(player1, player2);
		
		clearGameBoard();
		buildGridPane();
		
		ImageView centerTile =(ImageView) grid.getChildren().get(4+4*9); 
		initializeDragAndDrop(centerTile);
		
		return player1;
	}

	@Override
	public void onPlayerBeginRound(Player player) {
		updateRack(player);
		updatePointsCounter();
		
		if (GameManager.currentPlayer()== player1) {
			playerName.setText("Joueur 1");
			roundLeft--;
		}
		else
			playerName.setText("Joueur 2");
		
		
	}

	@Override
	public GameActionResult onPlayerTryPlaceTileAt(int row, int column, Tile tile) {
		if (Boolean.FALSE.equals(gameBoard.getPlaceHolderAt(4, 4).hasTile())) {
			if(row == 4 && column == 4)
				return GameActionResult.GRANTED;			
			else
				return GameActionResult.DENIED;
		}
		
		if(Boolean.FALSE.equals(gameBoard.validCoordinates(row, column)))
			return GameActionResult.DENIED;
		else if (checkColorAndType(row, column,tile)&& (GameManager.currentPlayer().points())>1)
			return GameActionResult.GRANTED;
		else
			return GameActionResult.DENIED;
	}


}
