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

async function render() {
    let treeReq = await fetch('./output/tree.json');
    let textReq = await fetch('./input.lan');
    let tree = await treeReq.json();
    let textResult = await textReq.text();
    let container = document.getElementById('container');

    let root = el('h1', text('Hello'));
    container.appendChild(root);

    console.log(tree);
    console.log(textResult);
}

document.addEventListener('DOMContentLoaded', render);
