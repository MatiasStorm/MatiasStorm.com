from rest_framework import serializers
from . import models

class PostCategorySerializer (serializers.ModelSerializer):
    class Meta:
        model = models.PostCategory
        fields = [ 
            "id" ,
            "category_name",
            "description",
            "created",
        ]

class SerieSerializer (serializers.ModelSerializer):
    class Meta:
        model = models.Serie
        fields = [ 
            "id",
            "serie_name",
            "description",
            "created",
        ]



class PostSerializer (serializers.ModelSerializer):
    class Meta:
        model = models.Post
        fields = [ 
            "id", 
            "title",
            "text",
            # "image_path",
            "categories",
            "serie",
            "published",
            "created",
            "updated"
        ]


