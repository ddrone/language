# This is going to be a script that will be used to update a file in Git
# repository easy with generated commit message.

import git
import os
import sys
from os import path


# Starting from directory, walk upwards until Git repository is found
def find_git_repo(root):
    folder = root
    while True:
        candidate = path.join(folder, '.git')
        if path.isdir(candidate):
            return folder

        # Break the loop if at root, otherwise continue
        if folder == path.dirname(folder):
            break
        folder = path.dirname(folder)

    raise Exception(f'no git repository found from {root}')

# Accepts absolute path for a file to be updated
def update_file(file):
    if not path.isfile(file):
        raise Exception(f'{file} is not a path to file')

    repo_dir = find_git_repo(path.dirname(file))
    rel_file = path.relpath(file, repo_dir)

    repo = git.Repo(repo_dir)
    if len(repo.index.diff('HEAD')) > 0:
        raise Exception(f'Repo {repo_dir} have staged files')

    # repo.index.add([file])
    message = f'Update {rel_file}'

def main():
    [_, file] = sys.argv
    update_file(path.abspath(file))


if __name__ == "__main__":
    main()
