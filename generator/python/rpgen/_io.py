"""Input/output helpers."""
# Copyright 2015 Andrew Dawson, Peter Dueben
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
from __future__ import absolute_import

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
