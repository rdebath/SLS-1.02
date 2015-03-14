#!mms -script

ADDHEAD NEWSGROUPS etl.test
SUBJECT mms test ＭＭＳでＭＩＭＥメッセージを作るテスト
ADDTEXT
I enclosed a sound of crash (
.
ADDTEXT bold
Sun's crash.au
.
ADDTEXT bold
)
.
ADDTEXT
.

ENCLOSE audio/basic /usr/demo/SOUND/sounds/crash.au
WRITE mms.out
