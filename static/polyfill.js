if (Array.prototype.forEach === undefined) {
    Array.prototype.forEach = function (f, thisArg) {
        f = thisArg === undefined ? f : f.bind(thiArg)
        for (var i = 0, e = this.length; i < e; ++i) {
            f(this[i], i, this)
        }
    }
}