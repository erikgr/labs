import java.awt.image.BufferedImage;
import java.io.IOException;


public class SkinDetect {
	
	BufferedImage target_image = null;
	int n_dilute_iterations = 3;
	double resize_factor = 1;
	
	public SkinDetect (BufferedImage target_image, int dilute_iterations, double resize_factor) {
		this.target_image = target_image;
		this.n_dilute_iterations = dilute_iterations;
		this.resize_factor = resize_factor;
	}
	
	public BufferedImage run () throws Exception {

		double[][] matrix = {
				{1,1,1},
				{1,1,1},
				{1,1,1}
		};

		BufferedImage resized = ImgUtil.rescale_nn(this.target_image,
				(int)(this.resize_factor * this.target_image.getWidth()),
				(int)(this.resize_factor * this.target_image.getHeight()));

		BufferedImage skin_mask = ImgUtil.skincolor_detection_mask(resized);

		for(int i=0;i<this.n_dilute_iterations;i++){

			BufferedImage tmp = ImgUtil.deep_copy(skin_mask);

			new ParallelFilter.ParallelFilterBuilder()
			.input(skin_mask)
			.output(tmp)
			.matrix(matrix)
			.create()
			.apply();

			skin_mask = tmp;
		}

		return ImgUtil.apply_mask(this.target_image,
				ImgUtil.rescale_nn(skin_mask,
						this.target_image.getWidth(),
						this.target_image.getHeight()));
	}
}

