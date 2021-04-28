function process(data) {
    console.log(data);
}

function el(tag, ...items) {
    let result = document.createElement(tag);
    items.forEach(e => {
        result.appendChild(e);
    });
    return result;
}

function text(t) {
    let result = el('span');
    result.innerText = t;
    return result;
}

class Renderer {
    constructor(tree, text) {
        this.tree = tree;
        this.text = text;
        this.highlightTarget = el('div');
    }

    render() {
        return el('div',
            this.renderTree(this.tree),
            this.highlightTarget
        );
    }

    renderTree(t) {
        console.log(t);
        let children = t.children.map(e => {
            return this.renderTree(e);
        });

        let button = el('button');
        button.innerText = t.treeType;
        button.addEventListener('click', () => {
            this.updateRange(t.range);
        });

        let result = el('div', button, ...children);
        result.classList.add('indent');
        return result;
    }

    updateRange(range) {
        let first = text(this.text.substring(0, range.startPos));
        let second = text(this.text.substring(range.startPos, range.endPos));
        second.classList.add('highlight');
        let third = text(this.text.substring(range.endPos));

        this.highlightTarget.innerHTML = '';
        this.highlightTarget.appendChild(el('div', first, second, third));
    }
}

async function render() {
    let headers = new Headers();
    headers.append('pragma', 'no-cache');
    headers.append('cache-control', 'no-cache');

    let treeReq = await fetch('./output/tree.json', {
        method: 'GET',
        headers
    });
    let textReq = await fetch('./input.lan', {
        method: 'GET',
        headers
    });
    let tree = await treeReq.json();
    let textResult = await textReq.text();
    let container = document.getElementById('container');

    let renderer = new Renderer(tree, textResult);
    container.appendChild(renderer.render());

    console.log(tree);
    console.log(textResult);
}

document.addEventListener('DOMContentLoaded', render);
