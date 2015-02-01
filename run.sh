# run.sh ---
# Filename: run.sh
# Author: Shi Yudi
# Created: 2015-01-03T10:44:31+0800
# Version:

# Commentary:
#
# 1. First run:
#
#   ./run.sh init
#
# 2. Generate website
#
#   ./run.sh
#
# 3. Test website
#
#   ./run.sh test
#
#

# Code:

USE_TIDY="TRUE"
CUSTOM_FILE="custom.el"

function elisp_func {
    emacs -Q --batch -L htmlize -l sydi-site.el -l custom.el -f "$1";
}

function elisp_code {
    emacs -Q --batch -L htmlize -l sydi-site.el -l custom.el --eval "$1";
}

function do_tidy {
    if [ "$USE_TIDY" ]; then
        find $PUBLISH_DIR -type f \( -name '*.html' -or -name '*.xml' \) -exec tidy -m {} \; 2>/dev/null
    fi
}

function usage {
    echo "Usage: ./run.sh [init|test]"
}

function check_init {
    if !(test -f $CUSTOM_FILE)
    then
        init; exit
    fi
}

function get_base_dir {
    # upper directory
    BASE_DIR_DEFAULT=$(dirname $(pwd))

    while [ true ]
    do
        echo -n "BASE_DIR ($BASE_DIR_DEFAULT): "
        read BASE_DIR
        if test -d $BASE_DIR -a -w $BASE_DIR
        then
            break
        fi
    done

    if [ "$BASE_DIR" = "" ]
    then
        echo "using default directory BASE_DIR: $BASE_DIR_DEFAULT"
        BASE_DIR=$BASE_DIR_DEFAULT
    fi
}

function init {
    get_base_dir

    ORIGIN_DIR=$BASE_DIR/origin
    PUBLISH_DIR=$BASE_DIR/publish

    mkdir -p $ORIGIN_DIR/{posts,dynamic,components,assets}

    echo '(setq org-publish-timestamp-directory "'$BASE_DIR'/.org-timestamps/")' >>$CUSTOM_FILE
    echo '(setq sydi-base-directory "'$ORIGIN_DIR'/")' >>$CUSTOM_FILE
    echo '(setq sydi-publish-directory "'$PUBLISH_DIR'/")' >>$CUSTOM_FILE
    echo '(setq sydi-site-url "http://YOUR_SITE_URL")' >>$CUSTOM_FILE
    echo '(setq sydi-site-name "YOUR_SITE_NAME")' >>$CUSTOM_FILE
    echo '(setq sydi-remotes '\''((:user "user" :host "host" :directory "/var/www")))' >>$CUSTOM_FILE

    echo "Write your javascript in file: $ORIGIN_DIR/assets/javascripts/site.js"
    echo "Write your css in file: $ORIGIN_DIR/assets/css/style.css"
    echo ""
    echo "
Congratulations, sydi-site init success!

  1. You can check your custom file $CUSTOM_FILE to make it your own.

  2. Then create your org articles under: $ORIGIN_DIR/posts/

  3. Come here and run ./run.sh to generate html files.

  4. Run ./run.sh test to see if your site create right.

  5. Run ./run.sh to publish your html files to remote machine if needed.
"
}

function main {
    check_init

    # where html files put
    PUBLISH_DIR=$(elisp_code '(message sydi-publish-directory)' 2>&1)

    case "$ACTION" in
        init)
            init
            ;;
        test)
            (
                cd $PUBLISH_DIR &&
                    (sleep 1 && xdg-open http://localhost:8000 &) &&
                    python2 -mSimpleHTTPServer 8000
            )
            ;;
        tidy)
            do_tidy
            ;;
        rsync)
            elisp_func sydi-remote-sync
            ;;
        "")
            elisp_func sydi-publish
            do_tidy
            ;;
        *)
            usage
            ;;
    esac
}

ACTION=$1
main

#
# run.sh ends here
