package latice.view;

import javafx.scene.Scene;
import javafx.stage.Screen;
import javafx.stage.Stage;

public abstract class ApplicationView {
	
	public static final ApplicationView HOME_VIEW = new HomeView();
	public static final ApplicationView GAME_VIEW = new GameView();
	
	//The stage used to show view
	private static Stage stage;
	
	//The current view
	private static ApplicationView currentView;
	
	/*
	 * The scene used for this view
	 */
	protected final Scene scene;
	
	/**
	 * If this view has been set-up
	 */
	protected boolean isSetUp = false;
	
	
	protected ApplicationView() {
		this.scene = initializeScene();
	}
	
	/**
	 * 
	 * @return The scene to be used by this view
	 */
	protected abstract Scene initializeScene();
	
	/**
	 * Set-up this view
	 * @return
	 * If the view has been set-up
	 * Note: The class implementing this should always return true
	 */
	protected abstract Boolean setUpView();
	
	/**
	 * Convert a length in pixel to match the screen resolution
	 * @param width A length in pixels
	 * @return The converted length
	 */
	protected final double fitToScreenWidth(double width) {
		Screen screen = Screen.getPrimary();
		return (width/screen.getDpi()/100)*screen.getBounds().getWidth();
	}
	
	/**
	 * Convert a length in pixel to match the screen resolution
	 * @param height A length in pixels
	 * @return The converted length
	 */
	protected final double fitToScreenHeight(double height) {
		Screen screen = Screen.getPrimary();
		return (height/screen.getDpi()/100)*screen.getBounds().getHeight();
	}
	
	/**
	 * Called when this view is shown
	 */
	protected void onShow() {}
	
	
	
	
	/**
	 * 
	 * @param stage The stage to use for showing views
	 */
	public static void stage(Stage stage) {ApplicationView.stage = stage;}
	
	/**
	 * 
	 * @return The stage used to show view
	 */
	public static Stage stage() {return ApplicationView.stage;}
	
	
	
	/**
	 * Show the view
	 * @param view The view to show
	 */
	public static void show(ApplicationView view) {
		ApplicationView.currentView = view;
		
		if(!ApplicationView.currentView.isSetUp)
			ApplicationView.currentView.isSetUp = ApplicationView.currentView.setUpView();
		
		ApplicationView.stage.hide();
		ApplicationView.stage.setScene(ApplicationView.currentView.scene);
		ApplicationView.currentView.onShow();
		ApplicationView.stage.show();
	}

}
