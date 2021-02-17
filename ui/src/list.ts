interface DataModel {
    items: string[]
}

function loadModel(): DataModel {
    let saved = window.localStorage.getItem('todo-graph');
    if (saved !== null) {
        return JSON.parse(saved);
    } else {
        return {items: ["modify me"]};
    }
}

function saveModel(model: DataModel) {
    window.localStorage.setItem('todo-graph', JSON.stringify(model));
}

function initUI() {
    console.log("Initializing UI");
    let host = document.querySelector('#target');
    let list = document.createElement('ul');
    let model = loadModel();

    function updateItem(i: number, item: string) {
        model.items[i] = item;
        saveModel(model);
    }

    function createElement(i: number): HTMLInputElement {
        let item = document.createElement('li');
        let input = document.createElement('input');
        input.value = model.items[i];
        item.appendChild(input);

        input.addEventListener('focusout', () => {
            updateItem(i, input.value);
        });

        list.appendChild(item);
        return input;
    }

    model.items.forEach((_, i) => {
        createElement(i);
    });

    let newItem = document.createElement('button');

    newItem.addEventListener('click', () => {
        let i = model.items.length;
        model.items.push('');
        let item = createElement(i);
        item.focus();
    });

    newItem.innerText = "Add item";
    host.appendChild(list);
    host.appendChild(newItem);
}

initUI();
