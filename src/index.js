import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
    node: document.getElementById('root')
});

window.addEventListener("scroll", () => {
    app.ports.onScroll.send([window.scrollX, window.scrollY]);
});

var elements = document.querySelectorAll('.elm-gallery-next');
for (var i = 0; i < elements.length; i++) {
    elements[i].style.backgroundColor = "black";
}

var elements1 = document.querySelectorAll('.elm-gallery-previous');
for (var i = 0; i < elements.length; i++) {
    elements1[i].style.backgroundColor = "black";
}

document.getElementById("image-gallery").style.backgroundColor = "black";
// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();