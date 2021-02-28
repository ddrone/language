import * as m from 'mithril';

var root = document.body;

interface Card {
    text: string;
    expected: string;
}

interface ReviewAttrs {
    cards: Card[];
}

interface ReviewState {
    currentCard: number;
}

const Review: m.Component<ReviewAttrs, ReviewState> = {
    oninit ({state}) {
        state.currentCard = 0;
    },
    view: function({attrs, state}) {
        if (state.currentCard < attrs.cards.length) {
            let card = attrs.cards[state.currentCard];
            return m("div", [
                m("p", card.text),
                m("textarea"),
                m("button", {
                    onclick: () => {
                        state.currentCard++;
                    }
                }, "Check")
            ]);
        }
        else {
            return m("div", "All done!");
        }
    }
}

const Root: m.Component = {
    view: function(_) {
        return m(Review, {cards: [
            {text: "This is a card text", expected: "answer"},
            {text: "This is a second card", expected: "second answer"}
        ]})
    }
}

m.mount(root, Root);
