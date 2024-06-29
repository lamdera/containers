/*

import Dict exposing (toList)
import Set exposing (toList)
import SeqDict exposing (toList)
import SeqSet exposing (toList)

*/

var _FNV_prime = 16777619;
var _FNV_offset = 2166136261;

function _FNV_hash(object) {
    switch (typeof object) {
    case 'string': return _FNV_hashString(object);
    case 'number': return _FNV_hashNum(object);
    case 'boolean': return object ? _FNV_hashNum(1) : _FNV_hashNum(0);
    case 'object': return _FNV_hashObj(object);
    default: return 0;
    }
}

function _FNV_hashString(str) {
    var current = _FNV_offset;

    for (var i = 0, len = str.length; i < len; i++) {
	current = (current ^ str.charCodeAt(i)) * _FNV_prime;
    }

    return current >>> 0;
}

function _FNV_hashNum(num) {
    return ((_FNV_offset ^ num) * _FNV_prime) >>> 0;
}

function _FNV_hashObj(obj) {
    var current = _FNV_offset;

    /**__DEBUG/
    if (obj.$ === 'Set_elm_builtin')
    {
        obj = __Set_toList(obj);
    }
    if (obj.$ === 'RBNode_elm_builtin' || obj.$ === 'RBEmpty_elm_builtin')
    {
        obj = __Dict_toList(obj);
    }
    if (obj.$ === 'SeqDict_elm_builtin')
    {
      obj = __SeqDict_toList(obj);
    }
    if (obj.$ === 'SeqSet_elm_builtin')
    {
      obj = __SeqSet_toList(obj);
    }
    //*/

    /**__PROD/
    if (obj.$ < 0)
    {
        if (obj.$ < -10)
        {
            obj = __SeqDict_toList(obj);
        }
        else
        {
            obj = __Dict_toList(obj);
        }
    }
    //*/

    for (var key in obj) {
	current = (current ^ _FNV_hash(obj[key])) * _FNV_prime;
    }

    return current >>> 0;
}

