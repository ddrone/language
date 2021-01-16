# This is going to be a script that will be used to update a file in Git
# repository easy with generated commit message.

import os
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


def main():
    folder = find_git_repo(os.getcwd())
    print(folder)


if __name__ == "__main__":
    main()
