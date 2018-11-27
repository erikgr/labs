import java.awt.image.BufferedImage;
import java.util.Vector;

public class ParallelFilter {
	
	private Vector<ConvolutionFilter> filters;
	
	public ParallelFilter(Vector<ConvolutionFilter> filters){
		this.filters = filters;
	}
	
	public static class ParallelFilterBuilder {
		
		BufferedImage input;
		BufferedImage output;
		int threads = Runtime.getRuntime().availableProcessors();
		double[][] matrix;
		
		public ParallelFilterBuilder input(BufferedImage input){
			this.input = input;
			return this;
		}
		
		public ParallelFilterBuilder output(BufferedImage output){
			this.output = output;
			return this;
		}
		
		public ParallelFilterBuilder matrix(double[][] matrix){
			this.matrix = matrix;
			return this;
		}
		
		public ParallelFilterBuilder threads(int t){
			this.threads = t;
			return this;
		}
		
		public ParallelFilter create () {
			
			Vector <ConvolutionFilter> filters = new Vector<ConvolutionFilter>();		
			
			for(int i=0; i<this.threads; i++){
				filters.add(new ConvolutionFilter
										.FilterBuilder()
										.input(this.input)
										.output(this.output)
										.matrix(this.matrix)
										.create());
			}
			
			return new ParallelFilter(filters);
		}
	}
	
	public void apply () {
		int step=this.filters.size();
		for(int start=0; start<step; start++){
			this.filters.get(start).apply(start, step);
		}
		int n_done = 0;
		while(n_done != step){
			n_done = 0;
			for(ConvolutionFilter cf : this.filters){
				if(cf.is_complete()){
					n_done++;
				}
			}
		}
	}
}
