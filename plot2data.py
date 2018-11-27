# For generating timeseries data
# by drawing them in images.

import sys
from PIL import Image

def image_to_timeseries(imgpath):
	img = Image.open(imgpath);
	hei = img.size[1];
	wid = img.size[0];
	pix = img.load();
	dat = [];
	for x in range(img.size[0]):
		for y in range(img.size[1]):
			if(pix[x,y][1]==0):
				dat.append(hei-y);
				break;
	sys.stdout.write(imgpath + " : ");
	for i in range(0,len(dat)):
		sys.stdout.write(str(dat[i]) + " ");
	sys.stdout.write("\n");

for idx in range(1,len(sys.argv)):
	image_to_timeseries(sys.argv[idx]);

