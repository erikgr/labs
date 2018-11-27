

public class GradientMagnitudeAndOrientationMapping {
	
	public int width;
	public int height;
	public double[][] magnitude;
	public double[][] orientation;
	
	public GradientMagnitudeAndOrientationMapping (double[][] mag, double[][] ori, int width, int height) {
		this.magnitude = mag;
		this.orientation = ori;
		this.width = width;
		this.height = height;
	}
}