import os
import sys
from datetime import datetime
from subprocess import run

name = datetime.utcnow().strftime("%Y%m%d-%H%M%S.md")
try:
    # -t stands for "topic"
    topic_index = sys.argv.index('-t')
    path = os.path.join(sys.argv[topic_index + 1], name)
except:
    path = name

run(['code', path])
