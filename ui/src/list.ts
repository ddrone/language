interface TreeItem {
    id: string,
    text: string,
    childen: TreeItem[],
    // Ids of items that are dependencies of current item
    dependencies: string[],
    // If true, children do not implicitly take this item as dependency
    group: boolean
}

class IdGenerator {
    letters: string;
    next_id: number;
    next_number: number;

    constructor() {
        this.letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        this.next_id = 0;
        this.next_number = 0;
    }
    
    next(): string {
        let result = this.letters[this.next_id] + this.next_number;
        if (this.next_id >= this.letters.length) {
            this.next_id = 0;
            this.next_number += 1;
        } else {
            this.next_id += 1;
        }
        return result;
    }

    createItem(text: string): TreeItem {
        return {
            id: this.next(),
            text: text,
            childen: [],
            dependencies: [],
            group: false,
        };
    }
}

interface DataModel {
    items: TreeItem[]
}

function loadModel(generator: IdGenerator): DataModel {
    let saved = window.localStorage.getItem('todo-graph');
    if (saved !== null) {
        return JSON.parse(saved);
    } else {
        return {items: [generator.createItem("modify me")]};
    }
}

function saveModel(model: DataModel) {
    window.localStorage.setItem('todo-graph', JSON.stringify(model));
}

function initUI() {
    console.log("Initializing UI");
    let host = document.querySelector('#target');
    let list = document.createElement('ul');
    let generator = new IdGenerator();
    let model = loadModel(generator);

    function updateItem(i: number, item: string) {
        model.items[i].text = item;
        saveModel(model);
    }

    function createElement(i: number): HTMLInputElement {
        let item = document.createElement('li');
        let input = document.createElement('input');
        input.value = model.items[i].text;
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
        model.items.push(generator.createItem(''));
        let item = createElement(i);
        item.focus();
    });

    newItem.innerText = "Add item";
    host.appendChild(list);
    host.appendChild(newItem);
}

initUI();
