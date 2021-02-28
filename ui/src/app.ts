import * as m from 'mithril';

var root = document.body;

interface ReviewAttrs {
    text: string;
    expected: string;
}

const Review: m.Component<ReviewAttrs> = {
    view: function(vnode) {
        return m("div", [
            m("p", vnode.attrs.text),
            m("textarea"),
            m("button", "Check")
        ]);
    }
}

m.render(root,
    m(Review, {text: "This is a card text", expected: "answer"})
);
