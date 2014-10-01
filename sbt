java  \
-Xmx1024M  \
-Xss2M  \
-XX:MaxPermSize=512m  \
-XX:+CMSClassUnloadingEnabled  \
-Djava.library.path="$LD_LIBRARY_PATH:./lib" \
-jar `dirname $0`/sbt-launch-0.13.5.jar "$@"
