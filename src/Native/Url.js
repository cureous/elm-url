/*

import Maybe exposing (Just, Nothing)

*/

var _proda_ai$elm_url$Native_Url = function(){
    function percentEncode(string)
    {
        return encodeURIComponent(string);
    }

    function percentDecode(string)
    {
        try
        {
            return _elm_lang$core$Maybe$Just(decodeURIComponent(string));
        }
        catch (e)
        {
            return _elm_lang$core$Maybe$Nothing;
        }
    }

    return {
        percentEncode: percentEncode,
        percentDecode: percentDecode
    };
}();
