package latice.application;



import javafx.application.Application;

import javafx.scene.image.Image;
import javafx.stage.Stage;
import latice.view.ApplicationView;

public class LaticeApplication extends Application{

	
	public static void main(String[] args) {
		Application.launch(args);
			
	}

	@Override
	public void start(Stage stage) throws Exception {
		Image icon = new Image(getClass().getResourceAsStream("/images/bg_sun.png"));
		
		ApplicationView.stage(stage);
		ApplicationView.show(ApplicationView.HOME_VIEW);
		
		
		stage.getIcons().add(icon);
		stage.setTitle("Latice");
		stage.setResizable(false);
		stage.show();
	}
	

}
