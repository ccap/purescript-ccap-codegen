/** @format */

exports.decodeArray_ = function(api) {
  return function(decode) {
    return function(ary) {
      if (Object.prototype.toString.call(ary) === '[object Array]') {
        const result = ary.slice();
        for (let i = 0; i < ary.length; i++) {
          const e = decode(ary[i]);
          if (api.isLeft(e)) {
            return e;
          } else {
            result[i] = api.fromRight()(e);
          }
        }
        return api.right(result);
      } else {
        return api.left('This value must be an array');
      }
    };
  };
};

exports.decodeString_ = function(api) {
  return function(json) {
    if (typeof json === 'string') {
      return api.right(json);
    } else {
      return api.left('This value must be a string');
    }
  };
};

exports.decodeBoolean_ = function(api) {
  return function(json) {
    if (typeof json === 'boolean') {
      return api.right(json);
    } else {
      return api.left('This value must be a boolean');
    }
  };
};

exports.decodeNumber_ = function(api) {
  return function(json) {
    if (typeof json === 'number') {
      return api.right(json);
    } else {
      return api.left('This value must be a number');
    }
  };
};

exports.decodeInt_ = function(api) {
  return function(json) {
    if (typeof json === 'number' && (json | 0) === json) {
      return api.right(json);
    } else {
      return api.left('This value must be an integer');
    }
  };
};

exports.decodeObject_ = function(api) {
  return function(json) {
    if (typeof json === 'object' && !Array.isArray(json) && json !== null) {
      return api.right(json);
    } else {
      return api.left('This value must be an object');
    }
  };
};

exports.lookup_ = function(api) {
  return function(prop) {
    return function(obj) {
      if (prop in obj) {
        return api.right(obj[prop]);
      } else {
        return api.left('Property ' + prop + ' does not exist');
      }
    };
  };
};

exports.isNull_ = function(json) {
  return json === null;
};
