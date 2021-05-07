import datetime
import os
import subprocess

reminder_path = os.path.expanduser('~/.reminder')
last_modified = datetime.datetime.fromtimestamp(
    os.stat(reminder_path).st_mtime, tz=datetime.timezone.utc)

now = datetime.datetime.now(tz=datetime.timezone.utc)

delta = now - last_modified
threshold = datetime.timedelta(hours=16)

if delta > threshold:
    subprocess.call(['cat', reminder_path])
    subprocess.call(['touch', reminder_path])
