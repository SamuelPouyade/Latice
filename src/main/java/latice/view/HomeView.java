package latice.view;

import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.image.Image;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundImage;
import javafx.scene.layout.BackgroundPosition;
import javafx.scene.layout.BackgroundRepeat;
import javafx.scene.layout.BackgroundSize;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;

public class HomeView extends ApplicationView {
	private static final Integer BTNWIDTH = 900;
	private static final Integer BTNHEIGHT = 800;

	@Override
	protected Scene initializeScene() {
		return new Scene(new BorderPane(), 600, 400);
	}

	@Override
	public Boolean setUpView() {
		
		BorderPane root = (BorderPane) this.scene.getRoot();
		
		//Top
		Text latice = new Text("Latice");
		latice.setStroke(Color.CHARTREUSE);
        latice.setStyle("-fx-font: 44 arial;");
        root.setTop(latice);
        BorderPane.setAlignment(latice, Pos.CENTER);
		
		//BACKGROUND
		Image fond = new Image(getClass().getResourceAsStream("/images/laticeBackground.png"));
        
		BackgroundImage backgroundImage = new BackgroundImage(fond,BackgroundRepeat.NO_REPEAT,
                BackgroundRepeat.NO_REPEAT,
                BackgroundPosition.DEFAULT,
                BackgroundSize.DEFAULT);
        
        Background background = new Background(backgroundImage);
        
        root.setBackground(background);
              
        
        
        
        
        
        
        //CENTER
        VBox vbox = new VBox();
		Button btnPlay = new Button("Jouer");
		Button btnLeave = new Button("Quitter");
		
		btnPlay.setPrefSize(fitToScreenWidth(BTNWIDTH), fitToScreenHeight(BTNHEIGHT));
		btnLeave.setPrefSize(fitToScreenWidth(BTNWIDTH), fitToScreenHeight(BTNHEIGHT));
		
		vbox.getChildren().addAll(btnPlay, btnLeave);
		vbox.setAlignment(Pos.CENTER);
		
		btnPlay.setOnMouseClicked(event -> 
			ApplicationView.show(GAME_VIEW));
		
		btnLeave.setOnMouseClicked(event -> 
			ApplicationView.stage().close());
		
		root.setCenter(vbox);
		
		return true;
	}
	
	@Override
	protected void onShow() {
		ApplicationView.stage().setMaximized(false);
	}

}
