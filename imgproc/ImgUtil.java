import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.WritableRaster;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Vector;

import javax.imageio.ImageIO;

/**
 * 
 * @author Neko
 * 
 * TODO: fourier transform
 *
 */
public class ImgUtil {


	public ImgUtil () {
		
	}
	
	
	public static BufferedImage from_url (String _url) throws MalformedURLException, IOException {
		BufferedImage img = null;
		img = ImageIO.read(new URL(_url));
		return img;
	}
	
	
	public static BufferedImage from_file (String _file) throws IOException {
		BufferedImage img = null;
		img = ImageIO.read(new File(_file));
		return img;
	}
	
	
	public static void to_file (BufferedImage img, String filename) throws IOException {
		ImageIO.write(img, "jpeg", new File(filename));
	}
	
	public static BufferedImage copy_blank (BufferedImage img) {
		return new BufferedImage(img.getWidth(), img.getHeight(), img.getType());
	}
	
	public static BufferedImage deep_copy(BufferedImage bi) {
		ColorModel cm = bi.getColorModel();
		boolean isAlphaPremultiplied = cm.isAlphaPremultiplied();
		WritableRaster raster = bi.copyData(null);
		return new BufferedImage(cm, raster, isAlphaPremultiplied, null);
	}
	
	public static void setR(BufferedImage img, int x, int y, int r_val) {
		int c = 0;
		c = ((r_val << 16) | (ImgUtil.getG(img, x, y) << 8) | ImgUtil.getB(img, x, y));
		img.setRGB(x, y, c);
	}
	
	
	public static void setG(BufferedImage img, int x, int y, int g_val) {
		int c = 0;
		c = ((ImgUtil.getR(img, x, y) << 16) | (g_val << 8) | ImgUtil.getB(img, x, y));
		img.setRGB(x, y, c);
	}
	
	
	public static void setB(BufferedImage img, int x, int y, int b_val) {
		int c = 0;
		c = ((ImgUtil.getR(img, x, y) << 16) | (ImgUtil.getG(img, x, y) << 8) | (b_val));
		img.setRGB(x, y, c);
	}
	
	
	public static void setRGB (BufferedImage img, int x, int y, int r, int g, int b) {
		int c = (r << 16) | (g << 8) | (b);
		img.setRGB(x, y, c);
	}
	
	
	public static int getR(BufferedImage img, int x, int y) {
		return (img.getRGB(x, y) >> 16 & 0xFF);
	}
	
	
	public static int getG(BufferedImage img, int x, int y) {
		return (img.getRGB(x, y) >> 8 & 0xFF);
	}
	
	
	public static int getB(BufferedImage img, int x, int y) {
		return (img.getRGB(x, y) & 0xFF);
	}
	
	
	public static BufferedImage aritmetic (BufferedImage img1, BufferedImage img2, String operation) {
		
		BufferedImage output = ImgUtil.copy_blank(img1);
		
		// eat shit and die
		//
		//if(img1.getType() != img2.getType()){
		//
		//
		//	System.out.println("Cannot process images: differing imagetypes!");
		//	return output;
		//}
		
		int threads = Runtime.getRuntime().availableProcessors();
		Vector<ParallelImageOperator> operators = new Vector<ParallelImageOperator>();
		
		for(int start=0; start<threads; start++){
			operators.add(new ParallelImageOperator(img1, img2, output, operation, start, threads));
		}
		
		int completed=0;
		while(completed != threads){
			completed=0;
			for(ParallelImageOperator op : operators){
				if(op.is_complete()){
					completed++;
				}
			}
		}
		
		return output;
	}
	
	public static BufferedImage to_grayscale (BufferedImage img) {
		
		BufferedImage gimg = new BufferedImage(
				img.getWidth(),
				img.getHeight(),
				BufferedImage.TYPE_BYTE_GRAY);
		Graphics2D gfx = gimg.createGraphics();
		gfx.drawImage(img, 0, 0, null);
		gfx.dispose();
		
		return gimg;
	}
	
	public static GradientMagnitudeAndOrientationMapping calculate_gradients (BufferedImage img) {
		
		BufferedImage target;

		if(img.getType() != BufferedImage.TYPE_BYTE_GRAY){
			target = ImgUtil.to_grayscale(img);
		} else {
			target = img;
		}

		int height	= target.getHeight();
		int width	= target.getWidth(); 
		
		double[][] mag = new double [height][width];
		double[][] ori = new double [height][width];
		
		// >:)
		//
		for(int r=1; r<height-1; r++){
			for(int c=1; c<width-1; c++){
				double m = Math.sqrt
						(
							(
								Math.pow
								(
									(double)
									(
										(target.getRGB(c+1, r) & 0xFF)
										-
										(target.getRGB(c-1, r) & 0xFF)
									)
									,
									2.0
								)							
							)
							+
							(
								Math.pow
								(
									(double)
									(
										(target.getRGB(c, r+1) & 0xFF)
										-
										(target.getRGB(c, r-1) & 0xFF)
									)
									,
									2.0
								)		
							)
						);
				
				double o = Math.atan2
						(
							(double)
							(
									(target.getRGB(c, r+1) & 0xFF)
									-
									(target.getRGB(c, r-1) & 0xFF)
							)
							,
							(double)
							(
									(target.getRGB(c+1, r) & 0xFF)
									-
									(target.getRGB(c-1, r) & 0xFF)
							)
						);
				
				mag[r][c] = m;
				ori[r][c] = o;
			}
		}
		
		return new GradientMagnitudeAndOrientationMapping (mag, ori, width, height);
	}
	
	
	/**
	 * basically this is just a
	 * image-wide SIFT descriptor
	 * of x*x x-orientation histograms.
	 * 
	 * this results in a x^3 dimensional vector.
	 * experimental, but seems to work for our
	 * purposes when x is low ( ~ 2,3,4).
	 * 
	 * @return
	 */
	public static int[] calculate_image_fingerprint (BufferedImage img, int x) {
		
		GradientMagnitudeAndOrientationMapping map;
		
		if(img.getType() != BufferedImage.TYPE_BYTE_GRAY){
			map = ImgUtil.calculate_gradients(ImgUtil.to_grayscale(img));
		} else {
			map = ImgUtil.calculate_gradients(img);
		}
		
		int fingerprint[] = new int[x*x*x];
		int width = img.getWidth();
		int height = img.getHeight();
		int i = 0;
		int w_hop = (int) (width / x);
		int h_hop = (int) (height / x);
		
		// >:)
		//
		for(int a=0; a<x; a++){
			for(int b=0; b<x; b++) {
				
				int h_start	= (a * h_hop);
				int h_end 	= ((a + 1) * h_hop);
				int w_start = (b * w_hop);
				int w_end = ((b + 1) * w_hop);
				
				double[] h = new double[x];
				
				for(int aa=(int)h_start; aa<h_end; aa++){
					for(int bb=(int)w_start; bb<w_end; bb++) {
						int bin = (int) ((map.orientation[aa][bb] == (Math.PI)) ? (x - 1)
								: Math.floor(((map.orientation[aa][bb] + Math.PI) / ((2 * Math.PI) / x))));
						h[bin] += map.magnitude[aa][bb];
					}
				}
				
				double sum = 0;
				for(double d : h){ sum += (d*d); }

				sum = Math.sqrt(sum);
				
				for(int e=0; e<h.length; e++){
					h[e] /= sum;
					fingerprint[i] = (int) (Math.round(512*h[e]));
					i++;
				};
			}
		}
		
		return fingerprint;
	}
	
	
	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static double euclidean_distance (int[] a, int[] b) {
		
		if(a.length != b.length){ return 99999; }

		double sum = 0;
		for(int i=0; i<a.length; i++){
			double d = a[i] - b[i];
			sum += Math.pow(d, 2.0);
		}
		sum = Math.sqrt(sum);
		
		return sum;
	}


	/**
	 * Nearest neighbor rescaling
	 * 
	 * @param input
	 * @param dst_width
	 * @param dst_height
	 * @return
	 */
	public static BufferedImage rescale_nn (BufferedImage input, int dst_width, int dst_height) {

		BufferedImage output = new BufferedImage(dst_width, dst_height, BufferedImage.TYPE_INT_RGB);

		if((input.getWidth() == dst_width) && (input.getHeight() == dst_height)){ return ImgUtil.deep_copy(input); } // bad! this is not a deep copy, fix me!

		int width = input.getWidth();
		int height = input.getHeight();
		double x_ratio = (double) width / (double) dst_width;
		double y_ratio = (double) height / (double) dst_height;

		for (int i=0; i<dst_height; i++) {
			for (int j=0; j<dst_width; j++) {
				double tempx = Math.floor((double) j * x_ratio);
				double tempy = Math.floor((double) i * y_ratio);
				output.setRGB(j, i, input.getRGB((int)tempx, (int)tempy));
			}
		}

		return output;
	}
	
	
	/**
	 * 
	 * @param input
	 * @return
	 */
    public static BufferedImage skincolor_detection (BufferedImage input) {

        BufferedImage output = new BufferedImage(input.getWidth(), input.getHeight(), BufferedImage.TYPE_INT_RGB);

        int width = input.getWidth();
        int height = input.getHeight();

        for(int y=0; y<height; y++){
                for(int x=0; x<width; x++){
                        int r = ImgUtil.getR(input, x, y);
                        int g = ImgUtil.getG(input, x, y);
                        int b = ImgUtil.getB(input, x, y);
                        boolean t1 = ((1.2f <= ((float) r / (float) g))
                                        && (((float) r / (float) g)) <= 3.4f);
                        boolean t2 = ((0.1f <= (((float) g + (float) b) / (float) r))
                                        && ((((float) g + (float) b) / (float) r) <= 1.9));
                        boolean t3 = ((0.1f <= (((float) b / (float) r)))
                                        && ((((float) b / (float) r)) <= 0.9f));
                        if ((t1 && t2 && t3)) {
                                output.setRGB(x, y, input.getRGB(x, y));
                        }
                }
        }

        return output;
    }


	/**
	 * 
	 * @param input
	 * @return
	 */
    public static BufferedImage skincolor_detection_mask (BufferedImage input) {

        BufferedImage output = new BufferedImage(input.getWidth(), input.getHeight(), BufferedImage.TYPE_INT_RGB);

        int width = input.getWidth();
        int height = input.getHeight();

        for(int y=0; y<height; y++){
                for(int x=0; x<width; x++){
                        int r = ImgUtil.getR(input, x, y);
                        int g = ImgUtil.getG(input, x, y);
                        int b = ImgUtil.getB(input, x, y);
                        boolean t1 = ((1.2f <= ((float) r / (float) g))
                                        && (((float) r / (float) g)) <= 3.4f);
                        boolean t2 = ((0.1f <= (((float) g + (float) b) / (float) r))
                                        && ((((float) g + (float) b) / (float) r) <= 1.9));
                        boolean t3 = ((0.1f <= (((float) b / (float) r)))
                                        && ((((float) b / (float) r)) <= 0.9f));
                        if ((t1 && t2 && t3)) {
                                ImgUtil.setR(output, x, y, 255);
                                ImgUtil.setG(output, x, y, 255);
                                ImgUtil.setB(output, x, y, 255);
                        }
                }
        }

        return output;
    }
    
    
    public static BufferedImage apply_mask (BufferedImage img, BufferedImage mask) throws Exception {
    	
    	BufferedImage out = ImgUtil.copy_blank(img);
    	
    	if(img.getWidth() != mask.getWidth() && img.getHeight() != mask.getHeight()){
    		throw new Exception("Can not apply mask: Image dimensions do not match!");
    	}
    	
    	for(int y=0; y<img.getHeight(); y++){
    		for(int x=0; x<img.getWidth(); x++){
    			if(ImgUtil.getR(mask, x, y) == 255){
    				out.setRGB(x, y, img.getRGB(x, y));
    			}
    		}
    	}
    	
    	return out;
    }



	/**
	 * TODO: This piece makes a whole lot of assumptions
	 * - image is in grayscale
	 * - image is 8bit / channel (1byte grayscale)
	 * 
	 * @param img
	 * @return
	 * @throws Exception
	 */
	public static char[][] to_ascii (BufferedImage img) throws Exception {
		
		char[][] output		= new char[img.getWidth()][img.getHeight()];
		char[] grayscale	= new char[] { '@', '#', 'M', '*', '+', ',', '.', ' '};
		//char[] grayscale	= new char[] { '@', '%', '#', '*', '+', '=', '-', ':', '.', ' '};
		//char[] grayscale = new char[] { '$', '@', 'B', '%', '8', '&', 'W', 'M', '#', '*', 'o', 'a', 'h', 'k', 'b', 'd', 'p', 'q', 'w', 'm', 'Z', 'O', '0', 'Q', 'L', 'C', 
'J', 'U', 'Y', 'X', 'z', 'c', 'v', 'u', 'n', 'x', 'r', 'j', 'f', 't', '/', '\\', '|', '(', ')', '1', '{', '}', '[', ']', '?', '-', '_', '+', '~', '<', '>', 'i', '!', 'l', 'I', ';', 
':', ',', '"', '^', '`', '\'', '.', ' '};
		int height			= img.getHeight();
		int width			= img.getWidth();
		int mask			= 255;
		
		for(int y=0; y<height; y++){
			for(int x=0; x<width; x++){
				int pixelval = (img.getRGB(x, y) & mask);
				int index = (int) (((double) pixelval / (double) mask) * grayscale.length);
				index = (index >= grayscale.length) ? (grayscale.length-1) : index;
				output[x][y] = grayscale[(int)index];
			}
		}

		return output;
	}
		
	/**
	 * Print image in ascii grayscale.
	 * Also resize image to 100x??? for convenient
	 * terminal output.
	 *  
	 * @param img
	 * @throws Exception
	 */
	public static void print_image (BufferedImage img, int _width) throws Exception {
		
		BufferedImage input = ImgUtil.to_grayscale(img);
		int width = input.getWidth();
		int height = input.getHeight();
		int dst_width = _width;
		double rsz_factor = (double) dst_width / (double) width;
		int dst_height = (int) (rsz_factor * height);
		BufferedImage rs = ImgUtil.rescale_nn(input, dst_width, dst_height);
		char[][] ascii = ImgUtil.to_ascii(rs);
		
		for(int y=0; y<dst_height; y++){
			for(int x=0; x<dst_width; x++){
				System.out.print(ascii[x][y]);
			}
			System.out.println("");
		}
	}
	
	
	public static void print_image (BufferedImage img) throws Exception {
		print_image(img, 100);
	}
}

