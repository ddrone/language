import glob
import os
import html

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

import http.server
import socketserver
import subprocess

with socketserver.TCPServer(("", 0), http.server.SimpleHTTPRequestHandler) as httpd:
    port = httpd.socket.getsockname()[1]
    subprocess.run(["xdg-open", f"http://localhost:{port}"])
    httpd.serve_forever()
