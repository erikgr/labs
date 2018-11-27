

import java.awt.image.BufferedImage;

public class ParallelImageOperator implements Runnable {

	private BufferedImage img1;
	private BufferedImage img2;
	private BufferedImage output;
	private String operation;
	private int start;
	private int step;
	private boolean complete = false;
	
	public ParallelImageOperator (BufferedImage img1, BufferedImage img2, BufferedImage output, String operation, int start, int step) {
		this.img1 = img1;
		this.img2 = img2;
		this.output = output;
		this.operation = operation;
		this.start = start;
		this.step = step;
		Thread t = new Thread(this);
		t.start();
	}
	
	
	private void subtract (int x, int y) {
		
		if(img1.getType() == BufferedImage.TYPE_INT_RGB){
			int r1 = ImgUtil.getR(img1, x, y);
			int g1 = ImgUtil.getG(img1, x, y);
			int b1 = ImgUtil.getB(img1, x, y);
			int r2 = ImgUtil.getR(img2, x, y);
			int g2 = ImgUtil.getG(img2, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int r = r1 - r2;
			int g = g1 - g2;
			int b = b1 - b2;
			r = (r > 255) ? 255 : r;
			g = (g > 255) ? 255 : g;
			b = (b > 255) ? 255 : b;
			r = (r < 0) ? 0 : r;
			g = (g < 0) ? 0 : g;
			b = (b < 0) ? 0 : b;
			ImgUtil.setRGB(output, x, y, r, g, b);
		}
		else if(img1.getType() == BufferedImage.TYPE_BYTE_GRAY) {
			int b1 = ImgUtil.getB(img1, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int b = b1 - b2;
			b = (b > 255) ? 255 : b;
			b = (b < 0) ? 0 : b;
			ImgUtil.setB(output, x, y, b);
		}
	}
	
	
	private void add (int x, int y) {
		if (img1.getType() == BufferedImage.TYPE_INT_RGB) {
			int r1 = ImgUtil.getR(img1, x, y);
			int g1 = ImgUtil.getG(img1, x, y);
			int b1 = ImgUtil.getB(img1, x, y);
			int r2 = ImgUtil.getR(img2, x, y);
			int g2 = ImgUtil.getG(img2, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int r = r1 + r2;
			int g = g1 + g2;
			int b = b1 + b2;
			r = (r > 255) ? 255 : r;
			g = (g > 255) ? 255 : g;
			b = (b > 255) ? 255 : b;
			r = (r < 0) ? 0 : r;
			g = (g < 0) ? 0 : g;
			b = (b < 0) ? 0 : b;
			ImgUtil.setRGB(output, x, y, r, g, b);
		}
		else if(img1.getType() == BufferedImage.TYPE_BYTE_GRAY) {
			int b1 = ImgUtil.getB(img1, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int b = b1 + b2;
			b = (b > 255) ? 255 : b;
			b = (b < 0) ? 0 : b;
			ImgUtil.setB(output, x, y, b);
		}
	}
	
	
	private void multiply (int x, int y) {
		if (img1.getType() == BufferedImage.TYPE_INT_RGB) {
			int r1 = ImgUtil.getR(img1, x, y);
			int g1 = ImgUtil.getG(img1, x, y);
			int b1 = ImgUtil.getB(img1, x, y);
			int r2 = ImgUtil.getR(img2, x, y);
			int g2 = ImgUtil.getG(img2, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int r = r1 * r2;
			int g = g1 * g2;
			int b = b1 * b2;
			r = (r > 255) ? 255 : r;
			g = (g > 255) ? 255 : g;
			b = (b > 255) ? 255 : b;
			r = (r < 0) ? 0 : r;
			g = (g < 0) ? 0 : g;
			b = (b < 0) ? 0 : b;
			ImgUtil.setRGB(output, x, y, r, g, b);
		}
		else if(img1.getType() == BufferedImage.TYPE_BYTE_GRAY) {
			int b1 = ImgUtil.getB(img1, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int b = b1 * b2;
			b = (b > 255) ? 255 : b;
			b = (b < 0) ? 0 : b;
			ImgUtil.setB(output, x, y, b);
		}
	}
	
	
	private void and (int x, int y) {
		if (img1.getType() == BufferedImage.TYPE_INT_RGB) {
			int r1 = ImgUtil.getR(img1, x, y);
			int g1 = ImgUtil.getG(img1, x, y);
			int b1 = ImgUtil.getB(img1, x, y);
			int r2 = ImgUtil.getR(img2, x, y);
			int g2 = ImgUtil.getG(img2, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int r = r1 & r2;
			int g = g1 & g2;
			int b = b1 & b2;
			r = (r > 255) ? 255 : r;
			g = (g > 255) ? 255 : g;
			b = (b > 255) ? 255 : b;
			r = (r < 0) ? 0 : r;
			g = (g < 0) ? 0 : g;
			b = (b < 0) ? 0 : b;
			ImgUtil.setRGB(output, x, y, r, g, b);
		}
		else if(img1.getType() == BufferedImage.TYPE_BYTE_GRAY) {
			int b1 = ImgUtil.getB(img1, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int b = b1 & b2;
			b = (b > 255) ? 255 : b;
			b = (b < 0) ? 0 : b;
			ImgUtil.setB(output, x, y, b);
		}
	}
	
	
	private void or (int x, int y) {
		if (img1.getType() == BufferedImage.TYPE_INT_RGB) {
			int r1 = ImgUtil.getR(img1, x, y);
			int g1 = ImgUtil.getG(img1, x, y);
			int b1 = ImgUtil.getB(img1, x, y);
			int r2 = ImgUtil.getR(img2, x, y);
			int g2 = ImgUtil.getG(img2, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int r = r1 | r2;
			int g = g1 | g2;
			int b = b1 | b2;
			r = (r > 255) ? 255 : r;
			g = (g > 255) ? 255 : g;
			b = (b > 255) ? 255 : b;
			r = (r < 0) ? 0 : r;
			g = (g < 0) ? 0 : g;
			b = (b < 0) ? 0 : b;
			ImgUtil.setRGB(output, x, y, r, g, b);
		}
		else if(img1.getType() == BufferedImage.TYPE_BYTE_GRAY) {
			int b1 = ImgUtil.getB(img1, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int b = b1 | b2;
			b = (b > 255) ? 255 : b;
			b = (b < 0) ? 0 : b;
			ImgUtil.setB(output, x, y, b);
		}
	}
	
	
	private void not (int x, int y) {
		if (img1.getType() == BufferedImage.TYPE_INT_RGB) {
			int r1 = ImgUtil.getR(img1, x, y);
			int g1 = ImgUtil.getG(img1, x, y);
			int b1 = ImgUtil.getB(img1, x, y);
			int r2 = ImgUtil.getR(img2, x, y);
			int g2 = ImgUtil.getG(img2, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int r = r1 ^ r2;
			int g = g1 ^ g2;
			int b = b1 ^ b2;
			r = (r > 255) ? 255 : r;
			g = (g > 255) ? 255 : g;
			b = (b > 255) ? 255 : b;
			r = (r < 0) ? 0 : r;
			g = (g < 0) ? 0 : g;
			b = (b < 0) ? 0 : b;
			ImgUtil.setRGB(output, x, y, r, g, b);
		}
		else if(img1.getType() == BufferedImage.TYPE_BYTE_GRAY) {
			int b1 = ImgUtil.getB(img1, x, y);
			int b2 = ImgUtil.getB(img2, x, y);
			int b = b1 ^ b2;
			b = (b > 255) ? 255 : b;
			b = (b < 0) ? 0 : b;
			ImgUtil.setB(output, x, y, b);
		}
	}
	
	
	public boolean is_complete(){
		return this.complete;
	}
	
	@Override
	public void run() {

		int height = this.img1.getHeight();
		int width = this.img1.getWidth();
		
		for (int y = start; y < height; y += step) {
			for (int x = 0; x < width; x++) {
				switch (this.operation) {
				case "+":
					add(x, y);
					break;
				case "-":
					subtract(x, y);
					break;
				case "*":
					multiply(x, y);
					break;
				case "|":
					or(x, y);
					break;
				case "&":
					and(x, y);
					break;
				case "^":
					not(x, y);
					break;
				}
			}
		}
		
		this.complete = true;
	}
}
