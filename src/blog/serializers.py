from rest_framework import serializers
from . import models

class PostCategorySerializer (serializers.ModelSerializer):
    class Meta:
        model = models.PostCategory
        fields = [ 
            "id" ,
            "category_name",
            "color",
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
    categories = PostCategorySerializer(many=True, read_only=True)

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

    def fix_validated_data(self, validated_data):
        text = validated_data.get("text", "")
        validated_data["text"] = text.replace("\r", "")
        return validated_data

    def update(self, instance, validated_data):
        validated_data = self.fix_validated_data(validated_data)
        return super().update(instance, validated_data)
    

    def create(self, validated_data):
        validated_data = self.fix_validated_data(validated_data)
        return super().create(validated_data)



