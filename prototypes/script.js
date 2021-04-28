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
    }

    renderTree(t) {
        console.log(t);
        let children = t.children.map(e => {
            return this.renderTree(e);
        });

        let result = el('div', text(t.treeType), ...children);
        result.classList.add('indent');
        return result;
    }
}

async function render() {
    let treeReq = await fetch('./output/tree.json');
    let textReq = await fetch('./input.lan');
    let tree = await treeReq.json();
    let textResult = await textReq.text();
    let container = document.getElementById('container');

    let renderer = new Renderer(tree, textResult);
    container.appendChild(renderer.renderTree(tree));

    console.log(tree);
    console.log(textResult);
}

document.addEventListener('DOMContentLoaded', render);
