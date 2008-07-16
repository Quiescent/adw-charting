#! /bin/bash

echo "sync with cl-user"
darcs push rdavis@common-lisp.net:/project/adw-charting/public_html/darcs/adw-charting
echo "Compile the help"
~/clbuild/clbuild lisp --load doc/make-docs.lisp
echo "Publish the help"
scp doc/index.html rdavis@common-lisp.net:/project/adw-charting/public_html/
scp doc/*.png rdavis@common-lisp.net:/project/adw-charting/public_html/
echo "Make the distribution tarball"
darcs dist -d adw-charting
echo "publish the distribution tarball"
scp adw-charting.tar.gz rdavis@common-lisp.net:/project/adw-charting/public_html/
echo "all done.  Now go blog and post to the mailing list."