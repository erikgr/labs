import java.io.IOException;

public class Img2ASCII {
	public static void main(String[] args) {
		
		int dst_width = 100;
		int n=0;
		
		// first argument can be considered output width
		if(args.length > 1){
			try {
				dst_width = Integer.parseInt(args[0]);
				n++;
			} catch (Exception e) {
				// ...
			}
		}
		
		try {
			ImgUtil.print_image_ANSIVT100_colors(ImgUtil.from_file(args[n]), dst_width);
		} catch (IOException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
