import * as m from 'mithril';

var root = document.body;

interface CardReview {
    card_index: number;
    deletion_index: number;
    rendered: string;
    answer: string;
    source: string;
}

interface CardsResponse {
    file_hash: string;
    reviews: CardReview[];
}

interface ReviewAttrs {
}

interface ReviewState {
    currentCard: number;
    loaded: boolean;
    response: CardsResponse;
    showSource: boolean;
    currentAnswer: string;
    showConfirmation: boolean;
}

const Review: m.Component<ReviewAttrs, ReviewState> = {
    oninit ({state}) {
        state.currentCard = 0;
        state.loaded = false;
        state.showSource = false;
        state.currentAnswer = '';
        state.showConfirmation = false;
        m.request({
            method: "GET",
            url: "http://localhost:31337/review"
        })
        .then((result) => {
            console.log(result);
            state.response = result as CardsResponse;
            state.loaded = true;
        })
    },
    view: function({attrs, state}) {
        if (!state.loaded) {
            return m("div", "Loading");
        }
        else if (state.currentCard < state.response.reviews.length) {
            let card = state.response.reviews[state.currentCard];
            return m("div", [
                m("p", m.trust(card.rendered)),
                // TODO: similar pattern should be in a helper library
                m("div", !state.showSource ? [] : [
                    m("pre", card.source)
                ]),
                m("textarea", {
                    oninput: (event: InputEvent) => {
                        state.currentAnswer = (event.target as HTMLTextAreaElement).value;
                    },
                    readonly: state.showConfirmation
                }),
                m("button", {
                    onclick: () => {
                        state.showConfirmation = true;
                    }
                }, "Check"),
                m("div", state.showSource ? [] : [
                    m("button", {
                        onclick: () => {
                            state.showSource = true;
                            alert(state.currentAnswer);
                        }
                    }, "Show source"),
                ]),
            ]);
        }
        else {
            return m("div", "All done!");
        }
    }
}

const Root: m.Component = {
    view: function(_) {
        return m(Review, {})
    }
}

m.mount(root, Root);
