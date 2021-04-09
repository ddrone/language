import glob
import os
import html
import subprocess
from pathlib import Path

files = glob.glob("*.md")
files.sort(key=lambda f: -os.stat(f).st_mtime)
files = files[:30]

document = '''
<!DOCTYPE HTML>
<style>
.container {
    width: 800px;
}
pre {
    white-space: pre-wrap;
}
</style>
<div class='container'>
'''
for f in files:
    document += f'<h1>{f}</h1>\n'
    contents = open(f, 'r').read()
    document += f'<pre>{html.escape(contents)}</pre>\n'
document += '</div>'

with open('index.html', 'w') as f:
    f.write(document)

subprocess.run(["xdg-open", f'file:///{Path("index.html").absolute()}'])
