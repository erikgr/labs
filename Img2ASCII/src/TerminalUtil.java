
public class TerminalUtil {
	
	
	/**
	 * 
	 * @param r
	 * @param g
	 * @param b
	 * @param max
	 * @return
	 */
	public static int rgb_to_ANSI_color (int r, int g, int b, int max) {
		int ANSI_VT100_MAX = 5;
		double _r, _g, _b;
		_r = (r / (double) max) * ANSI_VT100_MAX;
		_g = (g / (double) max) * ANSI_VT100_MAX;
		_b = (b / (double) max) * ANSI_VT100_MAX;
		double color_code = (16 + 36 * _r + 6 * _g + _b);
		
		/*
		System.out.println(r+ " " +g+ " " +b);
		System.out.println(_r+ " " +_g+ " " +_b);
		System.out.println("c: " + color_code);
		*/
		
		return (int) color_code;
	}
	
	public static String print_FG (int code) {
		String esc_seq = "\033[38;5;" + code + "m";
		return esc_seq;
	}
	
	public static String print_BG (int code) {
		String esc_seq = "\033[48;5;" + code + "m";
		return esc_seq;
	}
	
	public static String remove_ANSI_attributes () {
		return "\033[0m";
	}
}
