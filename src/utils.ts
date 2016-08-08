


///
/// get keys from a dictionary
///
export function keysFromDictionary(dico : any): string[] {
        var keySet: string[] = [];
        for (var prop in dico) {
            if (dico.hasOwnProperty(prop)) {
                keySet.push(prop);
            }
        }
        return keySet;
    }
