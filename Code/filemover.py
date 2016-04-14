import os
import shutil

src = "/Users/Yanish/Documents/winter_2016/Integ_275/maildir/allen-p"
dst = "/Users/Yanish/Documents/winter_2016/Integ_275/dest"


for filename in glob.glob(os.path.join(src, '*.*')):
    shutil.copy(filename, dst)