import java.awt.image.BufferedImage;

public class ConvolutionFilter extends AbstractFilter implements Runnable {

	private int kernel_size;
	private boolean complete = false;
	private int start = 0;
	private int step = 1;
	
	
	public ConvolutionFilter (double[][] matrix, int kernel_size, BufferedImage input, BufferedImage output) {
		this.matrix	= matrix;
		this.kernel_size = kernel_size;
		this.input	= input;
		this.output	= output;
	}
	
	
	public static class FilterBuilder {

		double[][] matrix;
		int kernel_size;
		BufferedImage input;
		BufferedImage output;
		
		
		public FilterBuilder matrix(double[][] matrix) {
			
			int a = matrix[0].length;
			int b = matrix[1].length;
			
			if((a == b) && (a % 2 == 1)){
				this.matrix = matrix;
				this.kernel_size = a;
			} else {
				System.out.println("ERROR: invalid filter matrix was skipped!");
			}
			
			return this;
		}
		
		
		public FilterBuilder input (BufferedImage input) {
			this.input = input;
			return this;
		}
		
		
		public FilterBuilder output (BufferedImage output) {
			this.output = output;
			return this;
		}
		
		
		public ConvolutionFilter create () {
			return new ConvolutionFilter(
					this.matrix,
					this.kernel_size,
					this.input,
					this.output);
		}
	}
	
	public void apply () {
		if(this.input.getColorModel().getNumComponents() == 3){
			applyRGB();
		} else if (this.input.getColorModel().getNumComponents() == 1){
			applyGray();
		}
	}
	
	
	public boolean is_complete () {
		return this.complete;
	}
	
	
	// for parallel filtering
	public void apply (int start, int step) {
		this.start = start;
		this.step = step;
		Thread t = new Thread(this);
		t.start();
	}
	
	
	private void applyRGB () {
		
        int ix, iy, fx, fy, wx, wy;
        int height	= this.input.getHeight();
		int width	= this.input.getWidth();
		
		for (iy = this.start; iy < height; iy += this.step) {
			for (ix = 0; ix < width; ix++) {
				
				double r = 0, g = 0, b = 0;

				for (fy = 0; fy < this.kernel_size; fy++) {
					for (fx = 0; fx < this.kernel_size; fx++) {
						wx = (ix - this.kernel_size / 2 + fx + width) % width;
						wy = (iy - this.kernel_size / 2 + fy + height) % height;
						r += ((double) ImgUtil.getR(this.input, wx, wy) * this.matrix[fx][fy]);
						g += ((double) ImgUtil.getG(this.input, wx, wy) * this.matrix[fx][fy]);
						b += ((double) ImgUtil.getB(this.input, wx, wy) * this.matrix[fx][fy]);
					}
				}
				
				r = (r > 255) ? 255 : r;
				g = (g > 255) ? 255 : g;
				b = (b > 255) ? 255 : b;
				
				r = (r < 0) ? 0 : r;
				g = (g < 0) ? 0 : g;
				b = (b < 0) ? 0 : b;
				
				ImgUtil.setR(output, (int) ix, (int) iy, (int) r);
				ImgUtil.setG(output, (int) ix, (int) iy, (int) g);
				ImgUtil.setB(output, (int) ix, (int) iy, (int) b);
			}
		}
	}
	
	
	private void applyGray () {
				
		int ix, iy, fx, fy, wx, wy;
        int height	= this.input.getHeight();
		int width	= this.input.getWidth();
		
		for (iy = this.start; iy < height; iy += this.step) {
			for (ix = 0; ix < width; ix++) {
				
				double gr = 0;

				for (fy = 0; fy < this.kernel_size; fy++) {
					for (fx = 0; fx < this.kernel_size; fx++) {
						wx = (ix - this.kernel_size / 2 + fx + width) % width;
						wy = (iy - this.kernel_size / 2 + fy + height) % height;
						gr += ((double) ImgUtil.getB(this.input, wx, wy) * this.matrix[fx][fy]);
					}
				}
				
				gr = (gr > 255) ? 255 : gr;
				gr = (gr < 0) ? 0 : gr;
				
				// blue channel in RGB is on the
				// same position as gray in grayscale
				ImgUtil.setB(output, (int) ix, (int) iy, (int) gr);
			}
		}
	}


	@Override
	public void run() {
		if(this.input.getColorModel().getNumComponents() == 3){
			applyRGB();
		} else if (this.input.getColorModel().getNumComponents() == 1) {
			applyGray();
		}
		this.complete = true;
	}
}
