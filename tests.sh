#!/bin/bash
# emacs -Q --batch -l ert.el -l oai-async1.el \
#   -l ./tests/oai-tests-async1.el -f ert-run-tests-batch-and-exit || exit 1
emacs -Q --batch -l ert.el -l oai-timers.el \
      -l ./tests/oai-tests-timers.el -f ert-run-tests-batch-and-exit || exit 1
emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el \
      -l ./tests/oai-tests-block.el -f ert-run-tests-batch-and-exit || exit 1
emacs -Q --batch -l ert.el -l oai-debug.el -l ../emacs-org-links/org-links.el -l oai-block.el -l oai-block-tags.el \
      -l ./tests/oai-tests-block-tags.el -f ert-run-tests-batch-and-exit || exit 1
emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l oai-block-tags.el -l oai-timers.el -l oai-async1.el -l oai-restapi.el \
      -l ./tests/oai-tests-restapi.el -f ert-run-tests-batch-and-exit || exit 1
emacs -Q --batch -l ert.el -l oai-debug.el -l oai-optional.el \
      -l ./tests/oai-tests-optional.el -f ert-run-tests-batch-and-exit || exit 1
emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l oai-block-tags.el -l oai-timers.el -l oai-async1.el -l oai-restapi.el -l oai-prompt.el \
      -l ./tests/oai-tests-prompt.el -f ert-run-tests-batch-and-exit || exit 1
emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l oai-block-tags.el -l oai-timers.el -l oai-async1.el -l oai-restapi.el -l oai-prompt.el -l oai.el \
      -l ./tests/oai-tests-oai.el -f ert-run-tests-batch-and-exit || exit 1
emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l oai-block-tags.el -l oai-timers.el -l oai-async1.el -l oai-restapi.el -l oai-prompt.el -l oai.el -l ./tests/oai-tests-block.el \
      -l ./tests/oai-tests-integ.el -f ert-run-tests-batch-and-exit || exit 1
emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l oai-block-tags.el -l oai-timers.el -l oai-async1.el -l oai-restapi.el -l oai-prompt.el -l oai.el \
      -l ./tests/oai-tests-integllm.el -f ert-run-tests-batch-and-exit || exit 1
