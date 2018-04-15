####################### \
Copy ./rscript/data to ./data since when knitting, the rootdir is set by the knitting document, hence relative path is not correct in the rscripts. The copied data in ./data is for the knitting documents.
####################### \

./data: # relative to the project root
	cp -r ./rscript/data ./data
	echo "Do not modify files in this directory, they are automatically generated from '/rscript/data' under the root directory of the project." > ./data/Do_Not_Modify.txt
