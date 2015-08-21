import json


def from_json(json_file, category_name, converter):
    with open(json_file, 'r') as f:
        raw_json = f.read()
    try:
        j = json.loads(raw_json)
    except ValueError:
        raise ValueError('The JSON is malformed')
    try:
        object_list = j[category_name]
    except KeyError:
        raise ValueError('The JSON must contain a top-level object '
                         'named "{}"'.format(category_name))
    return [converter(json_object) for json_object in object_list]
