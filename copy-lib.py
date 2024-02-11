import os

src = "/home/elff/R/x86_64-pc-linux-gnu-library/4.3"
dest = "lib/R/library/"

packages = os.listdir(src)

for pkg in packages:
    pkg_src = os.path.join(src,pkg)
    pkg_dest = os.path.join(dest,pkg)
    os.makedirs(pkg_dest,exist_ok=True)
    for item in os.listdir(pkg_src):
        src_item = os.path.join(pkg_src,item)
        dest_item = os.path.join(pkg_dest,item)
        #print(src_item)
        #print(dest_item)
        if not os.path.exists(dest_item):
            os.symlink(src_item,dest_item)
