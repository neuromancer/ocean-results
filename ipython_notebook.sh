#LD_LIBRARY_PATH=/usr/lib/jvm/java-7-openjdk-i386/jre/lib/i386/server:${LD_LIBRARY_PATH}
R CMD javareconf -e "ipython  notebook $1 --ip=0.0.0.0 --no-browser"
