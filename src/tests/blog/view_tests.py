import pytest
from django.urls import reverse

@pytest.fixture
def api_client():
    from rest_framework.test import APIClient
    return APIClient()


STRIPPED_POST_URL = reverse("blog:stripped-post-list")

@pytest.mark.django_db
class TestSprippedPostViewSet():
    def test_count(self, api_client, django_db_setup):
        count = 1
        param_string = "?count="
        response = api_client.get(STRIPPED_POST_URL + param_string + str( count ))
        assert response.status_code == 200
        assert len( response.data ) == count

        count = 5
        response = api_client.get(STRIPPED_POST_URL + param_string + str( count ))
        assert response.status_code == 200
        assert len( response.data ) == count

    def test_before(self, api_client, django_db_setup):
        param_string = "?before="
        date = "2020-11-08T18:48:45.825000" # Round to 825000, because test db does that.
        response = api_client.get(STRIPPED_POST_URL + param_string + date)
        assert len( response.data ) == 5

        date = "2019-11-08T18:48:45.825944"
        response = api_client.get(STRIPPED_POST_URL + param_string + date)
        assert len( response.data ) == 0

    def test_after(self, api_client, django_db_setup):
        param_string = "?after="
        date = "2020-11-08T18:48:45.825944"
        response = api_client.get(STRIPPED_POST_URL + param_string + date)
        assert len( response.data ) == 2

        date = "2021-11-08T18:48:45.825944"
        response = api_client.get(STRIPPED_POST_URL + param_string + date)
        assert len( response.data ) == 0

@pytest.mark.django_db
class TestPostViewSet():
    def test_count(self, api_client, django_db_setup):
        count = 1
        param_string = "?count="
        response = api_client.get(STRIPPED_POST_URL + param_string + str( count ))
        assert response.status_code == 200
        assert len( response.data ) == count

        count = 5
        response = api_client.get(STRIPPED_POST_URL + param_string + str( count ))
        assert response.status_code == 200
        assert len( response.data ) == count
