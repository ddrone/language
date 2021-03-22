from datetime import datetime
from subprocess import run

name = datetime.utcnow().strftime("%Y%m%d-%H%M%S.txt")
run(['code', name])